# Exporter
* export module local function

# quick start
* Add to rebar.config
```erlang
{deps, [
  ...
  {exporter, {git, "https://github.com/QCute/exporter.git", {branch, "master"}}}
]}.
```

* Usage 
```erlang
%% export it
exporter:export(io, to_tuple, 1),
%% call
io:to_tuple({}).
```

enjoy it~
