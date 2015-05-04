# conflag
A functional and declarative configuration language

Currently in a very messy state as I haven't decided 100% which language
implementation I prefer. The 1.0 milestone would support minimally
python, C, C++, and javascript, with easy bindings to other languages.

The goal of conflag is to provide a declarative configuration language
which allows for applying engineering best practices to your configurations,
such as DRY and modularity. It accomplishes this by supporting namespaces
and references, at the expense of easy programmatic serialization.

This makes it ideal for projects which have code-like and often complex
configurations, but not so much for eg. UI based configuration. Most tools
which load manually edited JSON configuration files would be easy targets
for improvement by switching to conflag, such as nginx or sublime text.

Since this is the use case I'm targetting, and JSON is already commonly used
for configuration, conflag is a legal superset of the JSON spec, meaning
if you currently use JSON for configuration you can switch to conflag today
(although as it is still very rough around the edges and middle I wouldn't
recommend it). You can also directly import JSON files in conflag files.
