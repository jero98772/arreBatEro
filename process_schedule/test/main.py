import heapq
import random
import threading
from collections import deque
from fastapi import FastAPI, Query
from fastapi.staticfiles import StaticFiles
from fastapi.responses import HTMLResponse
import json

class Task:
    def __init__(self, id, burst, children=None):
        self.id = id
        self.burst_time = burst
        self.children = children or []
        self.state = "start"
        self.ready_time = None
        self.start_time = None
        self.finish_time = None
        self.core_id = None
    
    def __lt__(self, other):
        return self.id < other.id
    
    def to_dict(self):
        return {
            'id': self.id,
            'burst_time': self.burst_time,
            'state': self.state,
            'ready_time': self.ready_time,
            'start_time': self.start_time,
            'finish_time': self.finish_time,
            'core_id': self.core_id,
            'children': [child.to_dict() for child in self.children]
        }

class WSDeque:
    def __init__(self):
        self.queue = deque()
        self.lock = threading.Lock()
    
    def push_bottom(self, task):
        with self.lock:
            self.queue.append(task)
    
    def pop_bottom(self):
        with self.lock:
            return self.queue.pop() if self.queue else None
    
    def steal_top(self):
        with self.lock:
            return self.queue.popleft() if self.queue else None

class MulticoreSimulator:
    def __init__(self, num_cores):
        self.num_cores = num_cores
        self.deques = [WSDeque() for _ in range(num_cores)]
        self.idle_cores = set(range(num_cores))
        self.event_queue = []
        self.time = 0
        self.event_counter = 0
        self.animation_data = []
        self.metrics = {
            "completed_tasks": 0,
            "steal_attempts": 0,
            "throughput": 0,
            "avg_waiting_time": 0,
            "avg_turnaround_time": 0,
            "finished_tasks": []
        }
    
    def schedule_event(self, time, event_type, *args):
        self.event_counter += 1
        heapq.heappush(self.event_queue, (time, self.event_counter, event_type, *args))
    
    def capture_animation_frame(self):
        frame = {
            'time': self.time,
            'cores': []
        }
        
        for core_idx in range(self.num_cores):
            core_data = {
                'id': core_idx,
                'idle': core_idx in self.idle_cores,
                'queue_size': len(self.deques[core_idx].queue),
                'tasks': []
            }
            
            # Get tasks in this core's queue
            for task in list(self.deques[core_idx].queue):
                core_data['tasks'].append(task.to_dict())
            
            frame['cores'].append(core_data)
        
        self.animation_data.append(frame)
    
    def run(self, initial_tasks):
        # Initialize with starting tasks
        for task in initial_tasks:
            self.schedule_event(0, "arrival", task)
        
        # Capture initial state
        self.capture_animation_frame()
        
        # Main event loop
        while self.event_queue:
            event = heapq.heappop(self.event_queue)
            time = event[0]
            self.time = time
            event_type = event[2]
            args = event[3:]
            
            if event_type == "arrival":
                task = args[0]
                core_idx = random.randint(0, self.num_cores - 1)
                task.state = "ready"
                task.ready_time = time
                task.core_id = core_idx
                self.deques[core_idx].push_bottom(task)
                if core_idx in self.idle_cores:
                    self.schedule_event(time, "schedule", core_idx)
            
            elif event_type == "finish":
                core_id, task = args
                task.state = "terminated"
                task.finish_time = time
                self.metrics["completed_tasks"] += 1
                self.metrics["finished_tasks"].append(task)
                
                # Process child tasks
                for child in task.children:
                    child.state = "ready"
                    child.ready_time = time
                    child.core_id = core_id
                    self.deques[core_id].push_bottom(child)
                
                self.schedule_event(time, "schedule", core_id)
            
            elif event_type == "schedule":
                core_id = args[0]
                if core_id not in self.idle_cores: 
                    continue
                    
                self.idle_cores.discard(core_id)
                task = self.deques[core_id].pop_bottom()
                
                if task:
                    task.state = "running"
                    task.start_time = time
                    task.core_id = core_id
                    finish_time = time + task.burst_time
                    self.schedule_event(finish_time, "finish", core_id, task)
                else:
                    # Work stealing phase
                    victims = [i for i in range(self.num_cores) if i != core_id]
                    random.shuffle(victims)
                    stolen = False
                    
                    for victim in victims:
                        task = self.deques[victim].steal_top()
                        self.metrics["steal_attempts"] += 1
                        if task:
                            stolen = True
                            task.state = "running"
                            task.start_time = time
                            task.core_id = core_id
                            finish_time = time + task.burst_time
                            self.schedule_event(finish_time, "finish", core_id, task)
                            break
                    
                    if not stolen:
                        self.idle_cores.add(core_id)
            
            # Capture animation frame after each event
            self.capture_animation_frame()
        
        self.calculate_metrics()
    
    def calculate_metrics(self):
        total_waiting = 0
        total_turnaround = 0
        
        for task in self.metrics["finished_tasks"]:
            waiting_time = task.start_time - task.ready_time
            turnaround_time = task.finish_time - task.ready_time
            total_waiting += waiting_time
            total_turnaround += turnaround_time
        
        if self.metrics["completed_tasks"] > 0:
            self.metrics["avg_waiting_time"] = total_waiting / self.metrics["completed_tasks"]
            self.metrics["avg_turnaround_time"] = total_turnaround / self.metrics["completed_tasks"]
        
        if self.time > 0:
            self.metrics["throughput"] = self.metrics["completed_tasks"] / self.time

def generate_workload(num_tasks, fork_prob=0.3):
    tasks = []
    for i in range(num_tasks):
        children = []
        if random.random() < fork_prob:
            children = [Task(f"{i}-{j}", random.randint(1, 5)) 
                        for j in range(random.randint(1, 3))]
        tasks.append(Task(i, random.randint(1, 10), children))
    return tasks

app = FastAPI()

# Mount static files
app.mount("/static", StaticFiles(directory="static"), name="static")

@app.get("/", response_class=HTMLResponse)
def read_root():
    html_content = open("templates/index.html").read()
    return HTMLResponse(content=html_content)

@app.get("/simulate")
def simulate(
    num_cores: int = Query(4, description="Number of CPU cores"),
    num_tasks: int = Query(20, description="Number of initial tasks"),
    fork_prob: float = Query(0.2, description="Probability of task forking")
):
    simulator = MulticoreSimulator(num_cores=num_cores)
    tasks = generate_workload(num_tasks, fork_prob=fork_prob)
    simulator.run(tasks)
    
    return {
        "animation_data": simulator.animation_data,
        "metrics": simulator.metrics,
        "total_time": simulator.time
    }