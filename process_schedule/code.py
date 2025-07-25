import heapq
import random
import threading
from collections import deque
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
from matplotlib.patches import Rectangle
from matplotlib.collections import PatchCollection
import matplotlib.cm as cm


class Task:
    def __init__(self, id: int, burst: int, children: list = None):
        self.id = id
        self.burst_time = burst
        self.children = children or []
        self.state = "start"  # Initial state
        self.ready_time = None  # Time when task became ready
        self.start_time = None  # Time when task started running
        self.finish_time = None  # Time when task finished
    
    def __lt__(self, other):
        return self.id < other.id

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
        self.metrics = {
            "completed_tasks": 0,
            "steal_attempts": 0,
            "throughput": 0,
            "avg_waiting_time": 0,
            "avg_turnaround_time": 0,
            "finished_tasks": []  # Store completed tasks for metrics
        }
    
    def schedule_event(self, time, event_type, *args):
        self.event_counter += 1
        heapq.heappush(self.event_queue, (time, self.event_counter, event_type, *args))
    
    def run(self, initial_tasks):
        # Initialize with starting tasks
        for task in initial_tasks:
            self.schedule_event(0, "arrival", task)
        
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
                # Update task state to READY
                task.state = "ready"
                task.ready_time = time
                self.deques[core_idx].push_bottom(task)
                if core_idx in self.idle_cores:
                    self.schedule_event(time, "schedule", core_idx)
            
            elif event_type == "finish":
                core_id, task = args
                # Update task state to TERMINATED
                task.state = "terminated"
                task.finish_time = time
                self.metrics["completed_tasks"] += 1
                self.metrics["finished_tasks"].append(task)
                
                # Process child tasks (set to READY state)
                for child in task.children:
                    child.state = "ready"
                    child.ready_time = time
                    self.deques[core_id].push_bottom(child)
                
                self.schedule_event(time, "schedule", core_id)
            
            elif event_type == "schedule":
                core_id = args[0]
                if core_id not in self.idle_cores: 
                    continue
                    
                self.idle_cores.discard(core_id)
                task = self.deques[core_id].pop_bottom()
                
                if task:
                    # Update task state to RUNNING
                    task.state = "running"
                    task.start_time = time
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
                            # Update stolen task state to RUNNING
                            task.state = "running"
                            task.start_time = time
                            finish_time = time + task.burst_time
                            self.schedule_event(finish_time, "finish", core_id, task)
                            break
                    
                    if not stolen:
                        self.idle_cores.add(core_id)
        
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

if __name__ == "__main__":
    simulator = MulticoreSimulator(num_cores=6)
    tasks = generate_workload(1000)
    simulator.run(tasks)
    
    print(f"Simulation completed in {simulator.time} units")
    print(f"Completed tasks: {simulator.metrics['completed_tasks']}")
    print(f"Throughput: {simulator.metrics['throughput']:.2f} tasks/unit")
    print(f"Avg waiting time: {simulator.metrics['avg_waiting_time']:.2f}")
    print(f"Avg turnaround time: {simulator.metrics['avg_turnaround_time']:.2f}")
    print(f"Steal attempts: {simulator.metrics['steal_attempts']}")
    
    # Print state transitions for first 5 tasks
    print("\nTask state transitions (first 5 tasks):")
    for task in simulator.metrics['finished_tasks'][:5]:
        print(f"Task {task.id}:")
        print(f"  State: {task.state}")
        print(f"  Ready at: {task.ready_time}")
        print(f"  Started at: {task.start_time}")
        print(f"  Finished at: {task.finish_time}")
        if task.children:
            print(f"  Generated {len(task.children)} child tasks")
