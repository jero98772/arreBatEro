import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

# Define the function to be animated
def sine_wave(x, t):
    return np.sin(x - t)

# Define the range of x values
x = np.linspace(0, 2*np.pi, 100)

# Create a figure and axis
fig, ax = plt.subplots()

# Create an empty plot object
line, = ax.plot(x, sine_wave(x, 0))

# Function to update the plot for each frame of the animation
def update(frame):
    line.set_ydata(sine_wave(x, frame))  # Update the y-data of the plot
    return line,

# Create the animation
ani = animation.FuncAnimation(fig, update, frames=np.linspace(0, 2*np.pi, 10),
                              blit=True)

plt.show()

