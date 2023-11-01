from core.constants_defines import *
from core.tools.world import World
from core.tools.voxel_marker import VoxelMarker
from core.tools.water import Water
import moderngl as mgl

class Scene:
    def __init__(self, app):
        self.app = app
        self.world = World(self.app)
        self.voxel_marker = VoxelMarker(self.world.voxel_handler)
        self.water = Water(app)

    def update(self):
        self.world.update()
        self.voxel_marker.update()
        #self.clouds.update()

    def render(self):
        # chunks rendering
        self.world.render()

        # rendering without cull face
        self.app.ctx.disable(mgl.CULL_FACE)
        self.water.render()
        self.app.ctx.enable(mgl.CULL_FACE)

        # voxel selection
        self.voxel_marker.render()



















