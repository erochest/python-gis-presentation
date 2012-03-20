% Introduction to Python for ArcGIS
% [Eric Rochester](mailto:erochest@virginia.edu)
% March 21--22, 2012

## Why Python?

* ESRI is moving everything to Python

### But...

* It's a nice language (although YMMV)
* Easy to learn
* Still powerful

## What Can You do with Python?

* Schedule jobs
* Batch processing
* Automate tasks and tools
* Loop through records
* Manipulate maps, layers, and geometries
* Use other Python packages (e.g., `numpy`)

## How do you Run Python?

* ArcMap Python Window
* Custom Toolbox
* Field Calculator
* [PythonWin][pythonwin]
* IDLE
* Command Line

# Python

## Variables and Types

### Strings

```python
a_string = 'One Place'
b_string = "Another Place"
```

### Numbers

```python
an_integer = 42
a_float = 3.1415
```

### Lists

```python
list1 = [0, 1, 1, 2, 3, 5]
list2 = [2.718, 3.141]
list3 = ['a', 'b', 'cdef']
```

## Expressions

```python
3 + 4
3 - 4

"Hello, " + "World"

7 * 3

21 / 7
22 / 7

22 % 7

22 / 7.0
22 / float(7)
```

## Operators

* addition (`+`)
* subtraction (`-`)
* multiplication (`*`)
* division (`/`)
* modulus (`%`)

## Statements, Part 1

### Assignment

```python
n1 = 3
n2 = n1 + 4
n2 = n2 + 7
```

### `import`

```python
import arcpy
import math
```

### `print`

```python
print "hello, world"
print n2
```

## Statements, Part 2

### Conditionals

```python
if n2 < 10:
    print 'too few'
elif n2 > 10:
    print 'too many'
else:
    print 'just right'
```

## Statements, Part 3

### Loops

```python
for item in list1:
    if (item % 2) == 0:
        print item, 'is even'
    else:
        print item, 'is odd'
```

## Modules

### Important GIS Modules

* arcpy
* arcpy.mapping
* arcpy.sa
* numpy
* Lots of others. [Batteries Included™][python26]

## Functions

First, unzip the data file into `C:\`.

```python
import arcpy
data = arcpy.Describe('C:/intro-python/charlottesville.gdb')
```

## Properties

```python

data.name

data.file

data.path

data.type
```

# Let's Do Something!

## Get Set up

1. Open a blank ArcMap.
2. Add a folder connection to `C:\intro-python`.
3. Drag `City_Trails` into the workspace.

## Let's Draw Some Buffers

```python
import arcpy
arcpy.env.workspace = 'C:/intro-python/charlottesville.gdb'

layer = 'City_Trails'
distances = ['100 meters', '200 meters', '400 meters']

for dist in distances:
    output = layer + '_' + dist.replace(' ', '_')
    arcpy.Buffer_analysis(layer, output, dist)
```

## Links

> * [Python][python]
> * [Learn Python the Hard Way][lphw] (For true beginners.)
> * [The Official Tutorial][tutorial] (This is best if you have some programming experience.)
> * [Python 2.6 Documentation][python26]
> * [PythonWin][pythonwin]
> * [ESRI's Python Page][esripy]
> * [arcpy][arcpy]
> * [intro-python.zip][data] (Data used here)

[python]: http://www.python.org/ "Python"
[python26]: http://docs.python.org/release/2.6.7/ "Python 2.6 Documentation"
[pythonwin]: http://wiki.python.org/moin/PythonWin "PythonWin"
[lphw]: http://learnpythonthehardway.org/ "Learn Python the Hard Way"
[tutorial]: http://docs.python.org/release/2.6.7/tutorial/index.html "The Python Tutorial"
[esripy]: http://www.esri.com/python "ESRI's Python Page"
[arcpy]: http://www.esri.com/arcpy "ArcPy"
[data]: intro-python.zip "intro-python.zip"

