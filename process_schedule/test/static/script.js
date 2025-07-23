class SchedulerAnimation {
    constructor() {
        this.animationData = [];
        this.currentFrame = 0;
        this.isPlaying = false;
        this.animationSpeed = 5;
        this.animationInterval = null;
        this.currentTasks = new Map(); // Track currently running tasks per core
        
        this.initializeElements();
        this.bindEvents();
    }
    
    initializeElements() {
        this.startBtn = document.getElementById('startBtn');
        this.pauseBtn = document.getElementById('pauseBtn');
        this.resetBtn = document.getElementById('resetBtn');
        this.speedSlider = document.getElementById('speedSlider');
        this.speedValue = document.getElementById('speedValue');
        this.currentTimeEl = document.getElementById('currentTime');
        this.currentFrameEl = document.getElementById('currentFrame');
        this.totalFramesEl = document.getElementById('totalFrames');
        
        // Metrics elements
        this.completedTasksEl = document.getElementById('completedTasks');
        this.stealAttemptsEl = document.getElementById('stealAttempts');
        this.throughputEl = document.getElementById('throughput');
        this.avgWaitingTimeEl = document.getElementById('avgWaitingTime');
        this.avgTurnaroundTimeEl = document.getElementById('avgTurnaroundTime');
        
        // Core elements
        this.coreWrappers = document.querySelectorAll('.core-wrapper');
    }
    
    bindEvents() {
        this.startBtn.addEventListener('click', () => this.startSimulation());
        this.pauseBtn.addEventListener('click', () => this.pauseAnimation());
        this.resetBtn.addEventListener('click', () => this.resetAnimation());
        
        this.speedSlider.addEventListener('input', (e) => {
            this.animationSpeed = parseInt(e.target.value);
            this.speedValue.textContent = `${this.animationSpeed}x`;
            if (this.isPlaying) {
                this.pauseAnimation();
                this.playAnimation();
            }
        });
    }
    
async startSimulation() {
    this.startBtn.disabled = true;
    this.startBtn.textContent = 'Loading...';
    
    // Get parameters from form inputs
    const numCores = document.getElementById('core-input').value;
    const numTasks = document.getElementById('task-input').value;
    const forkProb = document.getElementById('fork-input').value;
    
    // Build URL with query parameters
    const url = new URL('/simulate', window.location.origin);
    url.searchParams.append('num_cores', numCores);
    url.searchParams.append('num_tasks', numTasks);
    url.searchParams.append('fork_prob', forkProb);
    
    const response = await fetch(url);
    const data = await response.json();
    
    this.animationData = data.animation_data;
    this.metrics = data.metrics;
    this.totalFramesEl.textContent = this.animationData.length;
    
    this.updateMetrics();
    this.resetAnimation();
    this.playAnimation();
    
    this.startBtn.textContent = 'Start Simulation';
    this.startBtn.disabled = false;
    this.pauseBtn.disabled = false;
    this.resetBtn.disabled = false;
}    
    playAnimation() {
        if (this.animationData.length === 0) return;
        
        this.isPlaying = true;
        this.startBtn.disabled = true;
        this.pauseBtn.disabled = false;
        
        const frameDelay = Math.max(100, 1000 - (this.animationSpeed * 90));
        
        this.animationInterval = setInterval(() => {
            if (this.currentFrame >= this.animationData.length) {
                this.pauseAnimation();
                return;
            }
            
            this.renderFrame(this.animationData[this.currentFrame]);
            this.currentFrame++;
            this.currentFrameEl.textContent = this.currentFrame;
        }, frameDelay);
    }
    
    pauseAnimation() {
        this.isPlaying = false;
        if (this.animationInterval) {
            clearInterval(this.animationInterval);
            this.animationInterval = null;
        }
        
        this.startBtn.disabled = false;
        this.pauseBtn.disabled = true;
    }
    
    resetAnimation() {
        this.pauseAnimation();
        this.currentFrame = 0;
        this.currentFrameEl.textContent = '0';
        this.currentTimeEl.textContent = '0';
        this.currentTasks.clear();
        
        // Reset all cores to initial state
        this.coreWrappers.forEach((wrapper, coreId) => {
            this.updateCoreDisplay(wrapper, {
                id: coreId,
                idle: true,
                queue_size: 0,
                tasks: []
            }, null);
        });
    }
    
    renderFrame(frame) {
        this.currentTimeEl.textContent = frame.time.toFixed(2);
        
        frame.cores.forEach((core, index) => {
            const wrapper = this.coreWrappers[index];
            const runningTask = this.findRunningTask(core);
            this.updateCoreDisplay(wrapper, core, runningTask);
        });
    }
    
    findRunningTask(core) {
        // In this simulation, we need to determine the running task
        // based on the core's idle state and queue
        if (core.idle || core.tasks.length === 0) {
            return null;
        }
        
        // Find a task that should be running
        const runningTask = core.tasks.find(task => task.state === 'running');
        return runningTask || null;
    }
    
    updateCoreDisplay(wrapper, core, runningTask) {
        const coreId = core.id;
        
        // Update core info
        const statusEl = wrapper.querySelector('.core-status');
        const queueCountEl = wrapper.querySelector('.queue-count');
        const taskIdEl = wrapper.querySelector('.task-id');
        
        statusEl.textContent = core.idle ? 'Idle' : 'Busy';
        statusEl.className = core.idle ? 'core-status idle' : 'core-status busy';
        queueCountEl.textContent = core.queue_size;
        
        if (runningTask) {
            taskIdEl.textContent = runningTask.id;
        } else {
            taskIdEl.textContent = 'None';
        }
        
        // Update process state diagram
        this.updateProcessDiagram(wrapper, core, runningTask);
    }
    
    updateProcessDiagram(wrapper, core, runningTask) {
        const stateCircles = wrapper.querySelectorAll('.state-circle');
        const arrows = wrapper.querySelectorAll('.arrow');
        
        // Reset all states
        stateCircles.forEach(circle => circle.classList.remove('active'));
        arrows.forEach(arrow => arrow.classList.remove('active'));
        
        // Determine active states based on core status and tasks
        if (core.idle && core.queue_size === 0) {
            // Core is completely idle
            return;
        }
        
        if (core.queue_size > 0) {
            // There are tasks in ready state
            const readyCircle = wrapper.querySelector('.state-circle.ready');
            readyCircle.classList.add('active');
            
            // Animate new-to-ready arrow
            const newToReadyArrow = wrapper.querySelector('.arrow.new-to-ready');
            newToReadyArrow.classList.add('active');
        }
        
        if (runningTask) {
            // There's a running task
            const runningCircle = wrapper.querySelector('.state-circle.running');
            runningCircle.classList.add('active');
            
            // Animate ready-to-running arrow
            const readyToRunningArrow = wrapper.querySelector('.arrow.ready-to-running');
            readyToRunningArrow.classList.add('active');
            
            // Check if task is about to finish (simulate terminated state)
            if (Math.random() < 0.1) { // 10% chance to show terminated state
                const terminatedCircle = wrapper.querySelector('.state-circle.terminated');
                terminatedCircle.classList.add('active');
                
                const runningToTerminatedArrow = wrapper.querySelector('.arrow.running-to-terminated');
                runningToTerminatedArrow.classList.add('active');
            }
        }
        
        // Simulate waiting state occasionally
        if (!core.idle && Math.random() < 0.15) { // 15% chance
            const waitingCircle = wrapper.querySelector('.state-circle.waiting');
            waitingCircle.classList.add('active');
            
            const runningToWaitingArrow = wrapper.querySelector('.arrow.running-to-waiting');
            const waitingToReadyArrow = wrapper.querySelector('.arrow.waiting-to-ready');
            runningToWaitingArrow.classList.add('active');
            waitingToReadyArrow.classList.add('active');
        }
    }
    
    updateMetrics() {
        if (!this.metrics) return;
        
        this.completedTasksEl.textContent = this.metrics.completed_tasks;
        this.stealAttemptsEl.textContent = this.metrics.steal_attempts;
        this.throughputEl.textContent = this.metrics.throughput.toFixed(2);
        this.avgWaitingTimeEl.textContent = this.metrics.avg_waiting_time.toFixed(2);
        this.avgTurnaroundTimeEl.textContent = this.metrics.avg_turnaround_time.toFixed(2);
    }
}

// Enhanced animation effects
class AnimationEffects {
    static addTaskTransition(fromCore, toCore) {
        // Create a visual effect for task stealing
        const fromElement = document.querySelector(`[data-core="${fromCore}"]`);
        const toElement = document.querySelector(`[data-core="${toCore}"]`);
        
        if (fromElement && toElement) {
            const effect = document.createElement('div');
            effect.className = 'steal-effect';
            effect.style.cssText = `
                position: absolute;
                width: 10px;
                height: 10px;
                background: #e74c3c;
                border-radius: 50%;
                z-index: 1000;
                pointer-events: none;
                animation: stealAnimation 1s ease-in-out;
            `;
            
            document.body.appendChild(effect);
            
            setTimeout(() => {
                if (effect.parentNode) {
                    effect.parentNode.removeChild(effect);
                }
            }, 1000);
        }
    }
    
    static addCompletionEffect(coreElement) {
        // Add a completion burst effect
        const burst = document.createElement('div');
        burst.className = 'completion-burst';
        burst.style.cssText = `
            position: absolute;
            width: 100px;
            height: 100px;
            background: radial-gradient(circle, rgba(39, 174, 96, 0.6) 0%, transparent 70%);
            border-radius: 50%;
            top: 50%;
            left: 50%;
            transform: translate(-50%, -50%) scale(0);
            pointer-events: none;
            animation: burstAnimation 0.6s ease-out;
        `;
        
        coreElement.style.position = 'relative';
        coreElement.appendChild(burst);
        
        setTimeout(() => {
            if (burst.parentNode) {
                burst.parentNode.removeChild(burst);
            }
        }, 600);
    }
}

// Add CSS animations dynamically
const style = document.createElement('style');
style.textContent = `
    @keyframes stealAnimation {
        0% { transform: scale(1) translateX(0); opacity: 1; }
        50% { transform: scale(1.5) translateX(50px); opacity: 0.7; }
        100% { transform: scale(0.5) translateX(100px); opacity: 0; }
    }
    
    @keyframes burstAnimation {
        0% { transform: translate(-50%, -50%) scale(0); opacity: 1; }
        100% { transform: translate(-50%, -50%) scale(1); opacity: 0; }
    }
    
    .steal-effect {
        box-shadow: 0 0 10px rgba(231, 76, 60, 0.6);
    }
`;
document.head.appendChild(style);

// Initialize the application
document.addEventListener('DOMContentLoaded', () => {
    const scheduler = new SchedulerAnimation();
    
    // Add some interactive hover effects
    document.querySelectorAll('.state-circle').forEach(circle => {
        circle.addEventListener('mouseenter', () => {
            circle.style.transform = 'scale(1.1)';
        });
        
        circle.addEventListener('mouseleave', () => {
            circle.style.transform = 'scale(1)';
        });
    });
    
    // Add keyboard shortcuts
    document.addEventListener('keydown', (e) => {
        switch(e.key) {
            case ' ': // Spacebar
                e.preventDefault();
                if (scheduler.isPlaying) {
                    scheduler.pauseAnimation();
                } else {
                    scheduler.playAnimation();
                }
                break;
            case 'r':
            case 'R':
                scheduler.resetAnimation();
                break;
        }
    });
});

        function createCoreElement(coreId) {
            const coreWrapper = document.createElement('div');
            coreWrapper.className = 'core-wrapper';
            coreWrapper.dataset.core = coreId;
            coreWrapper.innerHTML = `
                <h3>Core ${coreId}</h3>
                <div class="process-diagram">
                    <div class="state-circle new">New</div>
                    <div class="state-circle ready">Ready</div>
                    <div class="state-circle running">Running</div>
                    <div class="state-circle waiting">Waiting</div>
                    <div class="state-circle terminated">Terminated</div>
                    <div class="arrows">
                        <div class="arrow new-to-ready"></div>
                        <div class="arrow ready-to-running"></div>
                        <div class="arrow running-to-waiting"></div>
                        <div class="arrow waiting-to-ready"></div>
                        <div class="arrow running-to-terminated"></div>
                    </div>
                </div>
                <div class="core-info">
                    <div class="status">Status: <span class="core-status">Idle</span></div>
                    <div class="queue-size">Queue: <span class="queue-count">0</span> tasks</div>
                    <div class="current-task">Current: <span class="task-id">None</span></div>
                </div>
            `;
            return coreWrapper;
        }

        // Function to initialize cores based on user input
        function initializeCores(numCores) {
            const coresContainer = document.getElementById('cores-container');
            coresContainer.innerHTML = ''; // Clear existing cores
            
            for (let i = 0; i < numCores; i++) {
                const coreElement = createCoreElement(i);
                coresContainer.appendChild(coreElement);
            }
        }

        // Initialize with default 4 cores
        document.addEventListener('DOMContentLoaded', () => {
            initializeCores(4);
            
            // Add input event listener to regenerate cores when core count changes
            document.getElementById('core-input').addEventListener('change', function() {
                const numCores = parseInt(this.value) || 4;
                initializeCores(numCores);
            });
        });