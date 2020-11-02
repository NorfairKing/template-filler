# Template Filler

This is a very simple little tool to replace placeholders in templates by your desired name.
It replaces all casings of a given string by the respective casings of your string.

Example invocation:

```
template-filler --source path/to/template --destination path/to/new/project --find FooBar --replace HomelessShelter
```

This replaces:

```
FooBar -> HomelessShelter
fooBar -> homelessShelter
foo-bar -> homeless-shelter
foo_bar -> homeless_shelter
FOO_BAR -> HOMELESS_SHELTER
foo bar -> homeless shelter
Foo Bar -> Homeless Shelter
foo.bar -> homeless.shelter
Foo.Bar -> Homeless.Shelter
foo/bar -> homeless/shelter
Foo/Bar -> Homeless/Shelter
```

If something goes wrong, you can find a backup of your files at `~/.local/share/template`.

