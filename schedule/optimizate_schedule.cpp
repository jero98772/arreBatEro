#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>
#include <map>
#include <iomanip>
#include <climits>
#include <sstream>

// Time utility functions
std::string minutesToTimeString(int minutes) {
    int hours = minutes / 60;
    int mins = minutes % 60;
    std::stringstream ss;
    ss << std::setw(2) << std::setfill('0') << hours << ":" 
       << std::setw(2) << std::setfill('0') << mins;
    return ss.str();
}

struct Activity {
    int start;
    int end;
    std::string name;
    bool isFixed;
    int priority;  // Higher number = higher priority
    
    Activity(int s, int e, std::string n, bool f, int p = 0) 
        : start(s), end(e), name(n), isFixed(f), priority(p) {}
    
    int duration() const { return end - start; }
    
    std::string toString() const {
        return name + " (" + (isFixed ? "Fixed" : "Flexible") + ", Priority: " + 
               std::to_string(priority) + "): " + 
               minutesToTimeString(start) + " - " + minutesToTimeString(end) + 
               " [Duration: " + std::to_string(duration()) + " mins]";
    }
};

struct Gap {
    int start;
    int end;
    
    Gap(int s, int e) : start(s), end(e) {}
    
    int duration() const { return end - start; }
    
    bool canFit(int taskDuration) const {
        return duration() >= taskDuration;
    }
};

// Task assignment strategies
enum class FitStrategy {
    BEST_FIT,     // Find smallest gap that fits the task
    FIRST_FIT,    // Use first gap that fits
    WORST_FIT     // Use largest gap that fits
};

// Comparator for tasks based on priority and duration
struct TaskComparator {
    bool operator()(const Activity& t1, const Activity& t2) {
        // First compare by priority (higher priority first)
        if (t1.priority != t2.priority) {
            return t1.priority < t2.priority;
        }
        // If same priority, compare by duration (longer tasks first)
        return t1.duration() < t2.duration();
    }
};

class ScheduleOptimizer {
private:
    std::vector<Activity> fixedActivities;
    std::vector<Activity> flexibleTasks;
    int dayStart;
    int dayEnd;
    FitStrategy strategy;
    
    std::vector<Gap> findGaps() const {
        std::vector<Gap> gaps;
        
        // Check if there's a gap before the first activity
        if (!fixedActivities.empty() && fixedActivities[0].start > dayStart) {
            gaps.emplace_back(dayStart, fixedActivities[0].start);
        } else if (fixedActivities.empty()) {
            // If no fixed activities, the whole day is a gap
            gaps.emplace_back(dayStart, dayEnd);
            return gaps;
        }
        
        // Find gaps between activities
        for (size_t i = 0; i < fixedActivities.size() - 1; ++i) {
            if (fixedActivities[i + 1].start > fixedActivities[i].end) {
                gaps.emplace_back(fixedActivities[i].end, fixedActivities[i + 1].start);
            }
        }
        
        // Check if there's a gap after the last activity
        if (!fixedActivities.empty() && fixedActivities.back().end < dayEnd) {
            gaps.emplace_back(fixedActivities.back().end, dayEnd);
        }
        
        return gaps;
    }
    
    // Find the best gap based on the current strategy
    std::vector<Gap>::iterator findBestGap(std::vector<Gap>& gaps, int taskDuration) {
        if (gaps.empty()) return gaps.end();
        
        switch (strategy) {
            case FitStrategy::BEST_FIT: {
                // Find smallest gap that fits (minimize wastage)
                int minWaste = INT_MAX;
                auto bestIt = gaps.end();
                
                for (auto it = gaps.begin(); it != gaps.end(); ++it) {
                    if (it->canFit(taskDuration)) {
                        int waste = it->duration() - taskDuration;
                        if (waste < minWaste) {
                            minWaste = waste;
                            bestIt = it;
                        }
                    }
                }
                return bestIt;
            }
            
            case FitStrategy::FIRST_FIT: {
                // Return first gap that can fit the task
                return std::find_if(gaps.begin(), gaps.end(), 
                                  [taskDuration](const Gap& g) { 
                                      return g.canFit(taskDuration); 
                                  });
            }
            
            case FitStrategy::WORST_FIT: {
                // Find largest gap that fits (maximize future flexibility)
                int maxSize = 0;
                auto bestIt = gaps.end();
                
                for (auto it = gaps.begin(); it != gaps.end(); ++it) {
                    if (it->canFit(taskDuration) && it->duration() > maxSize) {
                        maxSize = it->duration();
                        bestIt = it;
                    }
                }
                return bestIt;
            }
            
            default:
                return gaps.end();
        }
    }
    
    // Try to place task in best location within a gap
    bool tryPlaceTask(Activity& task, Gap& gap, std::vector<Activity>& schedule) {
        int taskDuration = task.duration();
        
        if (!gap.canFit(taskDuration)) {
            return false;
        }
        
        // For simplicity, place at start of gap
        task.start = gap.start;
        task.end = gap.start + taskDuration;
        
        // Add task to schedule
        schedule.push_back(task);
        
        // Update the gap
        gap.start += taskDuration;
        
        return true;
    }

public:
    ScheduleOptimizer(int start = 0, int end = 24 * 60, FitStrategy strat = FitStrategy::BEST_FIT) 
        : dayStart(start), dayEnd(end), strategy(strat) {}
    
    void addFixedActivity(const Activity& activity) {
        if (!activity.isFixed) {
            throw std::invalid_argument("Activity must be fixed");
        }
        fixedActivities.push_back(activity);
    }
    
    void addFlexibleTask(const Activity& task) {
        if (task.isFixed) {
            throw std::invalid_argument("Task must be flexible");
        }
        flexibleTasks.push_back(task);
    }
    
    void setFitStrategy(FitStrategy strat) {
        strategy = strat;
    }
    
    std::pair<std::vector<Activity>, std::vector<Activity>> optimize() {
        // Sort fixed activities by start time
        std::sort(fixedActivities.begin(), fixedActivities.end(), 
                  [](const Activity& a, const Activity& b) { return a.start < b.start; });
        
        // Check for overlaps in fixed activities
        for (size_t i = 0; i < fixedActivities.size() - 1; ++i) {
            if (fixedActivities[i].end > fixedActivities[i + 1].start) {
                std::cerr << "Warning: Fixed activities overlap! " 
                          << fixedActivities[i].name << " and " 
                          << fixedActivities[i + 1].name << std::endl;
            }
        }
        
        // Create priority queue for tasks
        std::priority_queue<Activity, std::vector<Activity>, TaskComparator> taskQueue;
        for (const auto& task : flexibleTasks) {
            taskQueue.push(task);
        }
        
        // Result schedule will contain fixed activities plus assigned tasks
        std::vector<Activity> finalSchedule = fixedActivities;
        std::vector<Activity> unscheduledTasks;
        
        // Find all gaps
        std::vector<Gap> gaps = findGaps();
        
        // Assign tasks to gaps
        while (!taskQueue.empty()) {
            Activity currentTask = taskQueue.top();
            taskQueue.pop();
            
            bool scheduled = false;
            for (auto& gap : gaps) {
                if (tryPlaceTask(currentTask, gap, finalSchedule)) {
                    scheduled = true;
                    break;
                }
            }
            
            // Remove empty gaps
            gaps.erase(std::remove_if(gaps.begin(), gaps.end(),
                                     [](const Gap& g) { return g.duration() <= 0; }),
                      gaps.end());
            
            if (!scheduled) {
                unscheduledTasks.push_back(currentTask);
            }
        }
        
        // Sort final schedule by start time
        std::sort(finalSchedule.begin(), finalSchedule.end(), 
                  [](const Activity& a, const Activity& b) { return a.start < b.start; });
        
        return {finalSchedule, unscheduledTasks};
    }
    
    // Analytics functions
    double calculateUtilization(const std::vector<Activity>& schedule) const {
        int totalScheduledTime = 0;
        for (const auto& activity : schedule) {
            totalScheduledTime += activity.duration();
        }
        
        int totalAvailableTime = dayEnd - dayStart;
        return static_cast<double>(totalScheduledTime) / totalAvailableTime;
    }
    
    int calculateTotalGapTime(const std::vector<Activity>& schedule) const {
        int totalGapTime = 0;
        int lastEndTime = dayStart;
        
        for (const auto& activity : schedule) {
            if (activity.start > lastEndTime) {
                totalGapTime += (activity.start - lastEndTime);
            }
            lastEndTime = std::max(lastEndTime, activity.end);
        }
        
        if (lastEndTime < dayEnd) {
            totalGapTime += (dayEnd - lastEndTime);
        }
        
        return totalGapTime;
    }
};

void printSchedule(const std::vector<Activity>& schedule, const std::string& title) {
    std::cout << title << ":" << std::endl;
    if (schedule.empty()) {
        std::cout << "  None" << std::endl;
        return;
    }
    
    for (const auto& activity : schedule) {
        std::cout << "  " << activity.toString() << std::endl;
    }
}

int main() {
    // Create scheduler with day from 7:00 to 19:00 (7am to 7pm)
    ScheduleOptimizer scheduler(7 * 60, 19 * 60, FitStrategy::BEST_FIT);
    
    // Add fixed activities
    scheduler.addFixedActivity(Activity(9 * 60, 10 * 60, "Morning Meeting", true));
    scheduler.addFixedActivity(Activity(12 * 60, 13 * 60, "Lunch", true));
    scheduler.addFixedActivity(Activity(15 * 60, 16 * 60, "Team Workshop", true));
    
    // Add flexible tasks with priorities (higher number = higher priority)
    scheduler.addFlexibleTask(Activity(0, 1, "Email", false, 5));
    scheduler.addFlexibleTask(Activity(0, 45, "Email", false, 1));
    scheduler.addFlexibleTask(Activity(0, 30, "Planning", false, 3));
    scheduler.addFlexibleTask(Activity(0, 60, "Code Review", false, 2));
    scheduler.addFlexibleTask(Activity(0, 150, "Coding Task", false, 4));
    scheduler.addFlexibleTask(Activity(0, 20, "Quick Call", false, 2));
    scheduler.addFlexibleTask(Activity(0, 25, "Documentation", false, 1));
    
    // Optimize schedule
    auto [scheduledActivities, unscheduledTasks] = scheduler.optimize();
    
    // Print results
    printSchedule(scheduledActivities, "Final Schedule");
    printSchedule(unscheduledTasks, "Unscheduled Tasks");
    
    // Print analytics
    std::cout << "\nSchedule Analytics:" << std::endl;
    std::cout << "  Time Utilization: " 
              << std::fixed << std::setprecision(2) 
              << scheduler.calculateUtilization(scheduledActivities) * 100 << "%" << std::endl;
    std::cout << "  Total Gap Time: " 
              << scheduler.calculateTotalGapTime(scheduledActivities) << " minutes" << std::endl;
    
    return 0;
}