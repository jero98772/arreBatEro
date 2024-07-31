import process_monitor

monitor = process_monitor.ProcessMonitor()

# List current processes
processes = monitor.getProcesses()
for process in processes:
    print(f"PID: {process.pid}, User: {process.user}, Name: {process.name}, Status: {process.status}, Memory: {process.memory} KB, CPU: {process.cpu_usage}%")

# Monitor processes (this will run indefinitely)
# monitor.monitorProcesses(5)
