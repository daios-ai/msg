
# Python Interface

MindScript can be embedded into Python programs, allowing you to leverage it as a library for processing Turing-complete workflows that incorporate LLMs.

## Embedding MindScript in Python

Before using MindScript in Python, ensure the `mindscript` library is installed via `pip`. Then, you can create an interpreter with your preferred LLM backend.

```python
import mindscript

# Create an LLM backend.
backend = mindscript.backend.Ollama(model="Phi4")

# Create an interpreter using the backend.
ip = mindscript.interpreter(backend=backend)

# Write a script.
code = '''
print("Hello, world!")
'''

# Execute the script.
ip.eval(code)
```

This prints `Hello, world!` to the standard output.

Currently, the available backends in `mindscript.backend` are:

- `LlamaCPP(self, url=None)`
- `Ollama(self, url=None, model=None)`
- `OpenAI(self, url=None, model=None, apikey=None)`


## Passing Data Between Python and MindScript

MindScript allows seamless data exchange with Python using JSON-compatible objects. These include Python's `bool`, `int`, `float`, `str`, `NoneType`, and the collection types `list` and `dict`.

MindScript's basic data types are encapsulated in `MValue` objects, which store both an annotation (`MValue.annotation`) and a value (`MValue.value`).

Some MindScript types, such as functions, oracles, and type definitions, do not have a direct Python equivalent and cannot be exported. For an `MValue` to be fully translatable, it must consist entirely of JSON-compatible values.

The MindScript interpreter processes `MObject` values, a superclass for all MindScript data types. If an `MObject` is an `MValue`, you can convert it into a native Python object using `mindscript.unwrap(obj: MValue)`. Conversely, Python-native JSON objects can be converted into MindScript values using `mindscript.wrap(obj)`.

### Retrieving Data from MindScript

The following example demonstrates how to define a record in MindScript and retrieve it in Python:

```python
# Create an LLM backend.
backend = mindscript.backend.Ollama(model="Phi4")

# Create an interpreter using the backend.
ip = mindscript.interpreter(backend=backend)

# Define a record in MindScript.
code = '''
let person = {
  first: "Albert",
  last: "Einstein",
  age: 76,
  genius: true
}
'''

# Execute the script and retrieve the result.
wrapped_obj = ip.eval(code)

# Unwrap the value.
obj = mindscript.unwrap(wrapped_obj)
```

Printing `obj` confirms that it is a Python-native JSON object:

```python
{'first': 'Albert', 'last': 'Einstein', 'age': 76, 'genius': True}
```

### Sending Data to MindScript

There are two ways to send JSON-compatible objects from Python to MindScript:

1. Using a JSON string.
2. Using variable definition.

#### Using a JSON String

You can serialize a JSON object as a string and execute it within the MindScript interpreter:

```python
import json
import mindscript

# Create an LLM backend and interpreter.
backend = mindscript.backend.Ollama(model="Phi4")
ip = mindscript.interpreter(backend=backend)

# Define a JSON object.
cities = [
  {'name': 'Santiago', 'population': 6903000},
  {'name': 'Zurich', 'population': 423193},
]

# Convert to JSON string.
cities_str = json.dumps(cities)

# Define the variable in MindScript.
code = f'let cities = {cities_str}'

# Execute the script.
ip.eval(code)
```

This creates a `cities` variable in MindScript containing the specified JSON object.

#### Using Variable Definition

Alternatively, you can wrap a JSON object into an `MValue` and define it within the interpreter's global scope:

```python
import mindscript

# Create an LLM backend and interpreter.
backend = mindscript.backend.Ollama(model="Phi4")
ip = mindscript.interpreter(backend=backend)

# Define a JSON object.
cities = [
  {'name': 'Santiago', 'population': 6903000},
  {'name': 'Zurich', 'population': 423193},
]

# Wrap the JSON object into an MValue.
wrapped_obj = mindscript.wrap(cities)

# Register it in the interpreter.
ip.define("cities", wrapped_obj)

# Verify its existence within the global scope.
ip.eval("println(cities)")
```

## Extending MindScript with Built-in Functions

MindScript allows the addition of custom built-in functions written in Python. These functions are accessible within the interpreter’s global scope.

To create a built-in function, extend the `MNativeFunction` class in `mindscript.objects` and implement the following methods:

- `__init__(self, ip)`: Initializes the function, registers its signature, and optionally provides documentation.
- `func(self, args: List[MObject])`: Implements the function logic. Arguments are type-checked MindScript objects. In case of an error, return `MValue(None, "error message")`.

The following example implements a native function for performing SQL queries:

```python
from mindscript import wrap
from mindscript.objects import MValue, MNativeFunction

# Create a class extending MNativeFunction
class SqlQuery(MNativeFunction):

  def __init__(self, ip):
    # Register function signature
    super().__init__(ip, 'fun(db: Str, query: Str) -> [Any]')
    # Provide function documentation
    self.annotation = (
      'Performs a SQL query on a database and returns an array of records.'
    )

  def func(self, args: List[MObject]):
    # Extract values (type-checked)
    db, query = args[0].value, args[1].value
    
    try:
      # Connect to the database, execute query, retrieve results
      ...
      return wrap(results)
    except sql.Error as err:
      return MValue(None, f'Database error: {err}')
```

### Registering the Function in the Interpreter

Once implemented, the function can be registered in the interpreter using `Interpreter.define`, making it available for use:

```python
# Create an LLM backend and interpreter.
backend = mindscript.backend.Ollama(model="Phi4")
ip = mindscript.interpreter(backend=backend)

# Register the built-in function.
ip.define('sqlQuery', SqlQuery(ip=ip))

# Execute the function within MindScript.
results = ip.eval('sqlQuery("my_db", "SELECT * FROM table1")')
```

This integration allows MindScript to interact seamlessly with external systems via Python-implemented functionality.

