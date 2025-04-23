import heapq
from enum import Enum
from datetime import datetime, timedelta

def minutes_to_time_string(minutes):
    hours, mins = divmod(minutes, 60)
    return f"{hours:02d}:{mins:02d}"

class Activity:
    def __init__(self, start, end, name, is_fixed, priority=0):
        self.start = start
        self.end = end
        self.name = name
        self.is_fixed = is_fixed
        self.priority = priority

    def duration(self):
        return self.end - self.start

    def __str__(self):
        fixed_str = 'Fixed' if self.is_fixed else 'Flexible'
        time_str = f"{minutes_to_time_string(self.start)} - {minutes_to_time_string(self.end)}"
        return f"{self.name} ({fixed_str}, Priority: {self.priority}): {time_str} [Duration: {self.duration()} mins]"

class Gap:
    def __init__(self, start, end):
        self.start = start
        self.end = end

    def duration(self):
        return self.end - self.start

    def can_fit(self, task_duration):
        return self.duration() >= task_duration

class FitStrategy(Enum):
    BEST_FIT = 1
    FIRST_FIT = 2
    WORST_FIT = 3

class ScheduleOptimizer:
    def __init__(self, start=0, end=24*60, strategy=FitStrategy.BEST_FIT):
        self.day_start = start
        self.day_end = end
        self.strategy = strategy
        self.fixed_activities = []
        self.flexible_tasks = []

    def add_fixed_activity(self, activity):
        if not activity.is_fixed:
            raise ValueError("Activity must be fixed")
        self.fixed_activities.append(activity)

    def add_flexible_task(self, task):
        if task.is_fixed:
            raise ValueError("Task must be flexible")
        self.flexible_tasks.append(task)

    def set_fit_strategy(self, strategy):
        self.strategy = strategy

    def find_gaps(self):
        fixed_sorted = sorted(self.fixed_activities, key=lambda a: a.start)
        gaps = []

        if not fixed_sorted:
            if self.day_start < self.day_end:
                gaps.append(Gap(self.day_start, self.day_end))
            return gaps

        if fixed_sorted[0].start > self.day_start:
            gaps.append(Gap(self.day_start, fixed_sorted[0].start))

        for i in range(len(fixed_sorted) - 1):
            current = fixed_sorted[i]
            next_act = fixed_sorted[i+1]
            if current.end < next_act.start:
                gaps.append(Gap(current.end, next_act.start))

        last = fixed_sorted[-1]
        if last.end < self.day_end:
            gaps.append(Gap(last.end, self.day_end))

        return gaps

    def try_place_task(self, task, gap, schedule):
        task_duration = task.duration()
        if not gap.can_fit(task_duration):
            return False
        task.start = gap.start
        task.end = gap.start + task_duration
        schedule.append(task)
        gap.start += task_duration
        return True

    def optimize(self):
        self.fixed_activities.sort(key=lambda a: a.start)

        for i in range(len(self.fixed_activities) - 1):
            current = self.fixed_activities[i]
            next_act = self.fixed_activities[i+1]
            if current.end > next_act.start:
                print(f"Warning: Fixed activities overlap! {current.name} and {next_act.name}")

        task_heap = []
        for task in self.flexible_tasks:
            heapq.heappush(task_heap, (-task.priority, -task.duration(), task))

        final_schedule = list(self.fixed_activities)
        unscheduled_tasks = []

        gaps = self.find_gaps()

        while task_heap:
            _, _, current_task = heapq.heappop(task_heap)
            scheduled = False

            for gap in gaps:
                if self.try_place_task(current_task, gap, final_schedule):
                    scheduled = True
                    break

            gaps = [gap for gap in gaps if gap.duration() > 0]

            if not scheduled:
                unscheduled_tasks.append(current_task)

        final_schedule.sort(key=lambda a: a.start)
        return (final_schedule, unscheduled_tasks)

    def calculate_utilization(self, schedule):
        total_scheduled = sum(act.duration() for act in schedule)
        total_available = self.day_end - self.day_start
        return total_scheduled / total_available if total_available != 0 else 0.0

    def calculate_total_gap_time(self, schedule):
        last_end = self.day_start
        total_gap = 0
        for act in sorted(schedule, key=lambda a: a.start):
            if act.start > last_end:
                total_gap += act.start - last_end
            last_end = max(last_end, act.end)
        if last_end < self.day_end:
            total_gap += self.day_end - last_end
        return total_gap

def print_schedule(schedule, title):
    print(f"{title}:")
    if not schedule:
        print("  None")
        return
    for activity in schedule:
        print(f"  {activity}")

if __name__ == "__main__":
    scheduler = ScheduleOptimizer(7*60, 19*60, FitStrategy.BEST_FIT)
    
    scheduler.add_fixed_activity(Activity(9*60, 10*60, "Morning Meeting", True))
    scheduler.add_fixed_activity(Activity(12*60, 13*60, "Lunch", True))
    scheduler.add_fixed_activity(Activity(15*60, 16*60, "Team Workshop", True))
    
    scheduler.add_flexible_task(Activity(0, 1, "Email", False, 5))
    scheduler.add_flexible_task(Activity(0, 45, "Email", False, 1))
    scheduler.add_flexible_task(Activity(0, 30, "Planning", False, 3))
    scheduler.add_flexible_task(Activity(0, 60, "Code Review", False, 2))
    scheduler.add_flexible_task(Activity(0, 150, "Coding Task", False, 4))
    scheduler.add_flexible_task(Activity(0, 20, "Quick Call", False, 2))
    scheduler.add_flexible_task(Activity(0, 25, "Documentation", False, 1))
    
    scheduled, unscheduled = scheduler.optimize()
    
    print_schedule(scheduled, "Final Schedule")
    print_schedule(unscheduled, "Unscheduled Tasks")
    
    print("\nSchedule Analytics:")
    utilization = scheduler.calculate_utilization(scheduled)
    total_gap = scheduler.calculate_total_gap_time(scheduled)
    print(f"  Time Utilization: {utilization:.2%}")
    print(f"  Total Gap Time: {total_gap} minutes")