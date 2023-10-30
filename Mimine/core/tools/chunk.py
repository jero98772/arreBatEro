from core.constants_defines import *
from core.meshes.chunk_mesh import ChunkMesh
import math


class Chunk:
    def __init__(self, world, position):
        self.app = world.app
        self.world = world
        self.position = position
        self.m_model = self.get_model_matrix()
        self.voxels: np.array = None
        self.mesh: ChunkMesh = None
        self.is_empty = True

    def get_model_matrix(self):
        m_model = glm.translate(glm.mat4(), glm.vec3(self.position) * CHUNK_SIZE)
        return m_model

    def set_uniform(self):
        self.mesh.program['m_model'].write(self.m_model)

    def build_mesh(self):
        self.mesh = ChunkMesh(self)

    def render(self):
        if not self.is_empty:
            self.set_uniform()
            self.mesh.render()

    def build_voxels(self):
        # empty chunk
        voxels = np.zeros(CHUNK_VOL, dtype='uint8')

        # fill chunk
        cx, cy, cz = glm.ivec3(self.position) * CHUNK_SIZE

        for x in range(CHUNK_SIZE):
            wx = x + cx+30
            for z in range(CHUNK_SIZE):
                wz = z + cz+30
                world_height = int(glm.simplex(glm.vec2(wx*0.5, wz*2) * 0.01) * 32 + 32)
                local_height = min(world_height - cy-32, CHUNK_SIZE)

                for y in range(CHUNK_SIZE):#range(local_height):
                    wy = y - cy
                    #voxels[x + CHUNK_SIZE * z + CHUNK_AREA * y] = wy + 1
                    if math.sqrt((x - CHUNK_SIZE / 2) * (x - CHUNK_SIZE / 2) + (y - CHUNK_SIZE / 2) * (y - CHUNK_SIZE / 2) + (z - CHUNK_SIZE / 2) * (z - CHUNK_SIZE / 2)) <= CHUNK_SIZE / 2:
                        voxels[x + CHUNK_SIZE * z + CHUNK_AREA * y] = wy + 1
                    #m_blocks[x][y][z].SetBlockType(BlockType_Grass);

        if np.any(voxels):
            self.is_empty = False

        return voxels























