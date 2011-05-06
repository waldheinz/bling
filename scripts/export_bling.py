#!BPY

"""
Name: 'Bling (.bling)...'
Blender: 249
Group: 'Export'
Tooltip: 'Save a Bling File'
"""

import Blender
from Blender import Mesh, Scene, Window, sys, Image, Draw, Types, Lamp
import math

def writeMatrix(o, m) :
   m1 = m.copy().transpose()
   o.write("transform {\n")
   o.write("   identity\n")
   o.write("   matrix {\n")
   o.write("      m %.6f %.6f %.6f %.6f\n" % (m1[0][0], m1[0][1], m1[0][2], m1[0][3]))
   o.write("      m %.6f %.6f %.6f %.6f\n" % (m1[1][0], m1[1][1], m1[1][2], m1[1][3]))
   o.write("      m %.6f %.6f %.6f %.6f\n" % (m1[2][0], m1[2][1], m1[2][2], m1[2][3]))
   o.write("      m %.6f %.6f %.6f %.6f\n" % (m1[3][0], m1[3][1], m1[3][2], m1[3][3]))
   i = m.copy().invert().transpose()
   o.write("      i %.6f %.6f %.6f %.6f\n" % (i[0][0], i[0][1], i[0][2], i[0][3]))
   o.write("      i %.6f %.6f %.6f %.6f\n" % (i[1][0], i[1][1], i[1][2], i[1][3]))
   o.write("      i %.6f %.6f %.6f %.6f\n" % (i[2][0], i[2][1], i[2][2], i[2][3]))
   o.write("      i %.6f %.6f %.6f %.6f\n" % (i[3][0], i[3][1], i[3][2], i[3][3]))
   o.write("   }\n")
   o.write("}\n")
   
def writeCamera(o, scene) :
   camobj = scene.objects.camera
   camera = Blender.Camera.Get()[0]
   
   o.write("transform { lookAt {\n")
   mat = camobj.getMatrix()
      
   # eye position can just be read out of the matrix
   w = mat[3][3]
   eye = (mat[3][0] / w, mat[3][1] / w, mat[3][2] / w)
   o.write("   pos %f %f %f\n" % eye)
   
   # get the dir vector (camera looking down z)
   d = (mat[2][0], mat[2][1], mat[2][2])
   
   print (d)
   print (d[0])
   
   # look is just the eye position - the direction
   look = (eye[0] - d[0], eye[1] - d[1], eye[2] - d[2])
   o.write("   look %f %f %f\n" % d)
   
   # up vector can just be read out of the matrix (y axis)
   up = (mat[1][0], mat[1][1], mat[1][2])
   o.write("   up %f %f %f\n" % up)
   o.write("}}\n\n")
   
   o.write("camera { perspective ")
   factor = 1
   fov = (360.0 * math.atan(factor * 16.0 / camera.getLens() ) / math.pi)
   o.write("fov %f " % fov)
  # o.write("fov %f\n" % fov)
   
   o.write("lensRadius 0 focalDistance 10 }\n")
          
def writeMaterial(o, mat) :
   o.write("material {\n")
   o.write("   plastic\n")
   
   d = mat.rgbCol
   o.write("   kd { constant rgb %f %f %f }\n" % (d[0], d[1], d[2]))

   d = mat.mirCol   
   o.write("   ks { constant rgb %f %f %f }\n" % (d[0], d[1], d[2]))
   o.write("   rough { constant 0.1 }\n")
   o.write("}\n")
          
def writeDefaultMaterial(o) :
   o.write("material { matte kd { constant ")
   o.write("rgb 0.8 0.8 0.8  } ")
   o.write("sigma { constant 0.0 } }\n")

def writeMesh(o, obj) :
   o.write("# Mesh " + obj.name + "\n")
   mesh = obj.getData()
   verts = mesh.verts
   faces = mesh.faces
   
   writeMatrix(o, obj.getMatrix())
   
   if len (mesh.materials) > 0 :
      mat = mesh.materials[0]
      writeMaterial(o, mat)
   else :
      writeDefaultMaterial(o)
   
   o.write("shape {\n")
   o.write("   mesh\n")
   o.write("   vertexCount %u\n" % len(verts))
   o.write("   faceCount %u\n" % len(faces))
      
   for v in verts :
      o.write("   v %.6f %.6f %.6f\n" % (v[0], v[1], v[2]))
   
   for face in faces:
		face_v = list(face.v)
		face_v.reverse()
		o.write("   f")
		
		for f in face_v :
		   o.write(" %i" % f.index)
		   
		o.write("\n")
		
		
   o.write("}\n")
		
def writeLamp(o, lamp) :
   if lamp.getType() == Lamp.Types.Area :
      o.write("# area lamp " + str(dir(lamp)) + "\n")
      
   else :
      o.write("# unsupported lamp type " + str(lamp) + "\n")
   
def write(filename, scene) :
   out = open(filename, "w")
   writeCamera(out, scene)
   
   objects = scn.objects
   
   for o in objects:
      if o.type == "Mesh" :
         writeMesh(out, o)
      elif o.type == "Lamp" :
         out.write("# Lamp " + o.name + "\n")
         writeLamp(out, o.getData())
      else :
         out.write("# skipped " + o.type + ": " + o.name + "\n")
        
   out.close()

# Blender.Window.FileSelector(write, "Export")

scn = Scene.GetCurrent()

write("/home/trem/test.bling", scn)

