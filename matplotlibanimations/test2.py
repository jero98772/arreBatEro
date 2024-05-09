import matplotlib.pyplot as plt
import numpy as np
from matplotlib.animation import FuncAnimation
import sys

# Initialize the game map
game_map = np.zeros((10, 10))

# Player position
player_pos = [5, 5]

# Set exit position
exit_pos = [0, 9]

# Set enemy position
enemy_pos = [8, 3]

# Set player direction
player_dir = 'up'

# Keyboard input handling
def on_key(event):
    global player_dir
    if event.key == 'up':
        player_dir = 'up'
    elif event.key == 'down':
        player_dir = 'down'
    elif event.key == 'left':
        player_dir = 'left'
    elif event.key == 'right':
        player_dir = 'right'
    elif event.key == 'escape':
        sys.exit(0)

# Update function for animation
def update(frame):
    global player_pos
    if player_dir == 'up' and player_pos[0] > 0:
        player_pos[0] -= 1
    elif player_dir == 'down' and player_pos[0] < 9:
        player_pos[0] += 1
    elif player_dir == 'left' and player_pos[1] > 0:
        player_pos[1] -= 1
    elif player_dir == 'right' and player_pos[1] < 9:
        player_pos[1] += 1

    # Check if player reaches the exit or the enemy
    if player_pos == exit_pos:
        print("You Win!")
        sys.exit(0)
    elif player_pos == enemy_pos:
        print("Game Over!")
        sys.exit(0)

    # Update plot
    ax.clear()
    ax.plot(player_pos[1], player_pos[0], 'bo', markersize=15)  # Player
    ax.plot(exit_pos[1], exit_pos[0], 'go', markersize=15)  # Exit
    ax.plot(enemy_pos[1], enemy_pos[0], 'ro', markersize=15)  # Enemy
    ax.set_xlim(0, 10)
    ax.set_ylim(0, 10)
    ax.set_aspect('equal')

# Set up the plot
fig, ax = plt.subplots()
fig.canvas.mpl_connect('key_press_event', on_key)

# Run the animation
ani = FuncAnimation(fig, update, frames=np.arange(0, 100), interval=200)
plt.show()

