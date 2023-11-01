from core.meshes.quad_mesh import QuadMesh
from core.constants_defines import *


class Water:
    def __init__(self, app):
        self.app = app
        self.mesh = QuadMesh(app)

    def render(self):
        self.mesh.render()
