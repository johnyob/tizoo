# mlsus ඞ

> A sus-piciously simple ML featuring constraint-based type inference with suspended constraints -- no impostors here!

`mlsus` is a work-in-progress implementation of a constraint-based type checker for an ML language with constructor disambiguation. It introduces a novel approach by utilising *suspended constraints* to achieve this functionality.

## Getting Started

> [!NOTE]
> `mlsus` is built with Nix, a package manager and system configuration tool that makes building from sources easy! See the [Nix docs](https://nixos.org/download/) for instructions for your system. Additionally, ensure [Nix flakes are enabled](https://nixos.wiki/wiki/Flakes#Enable_flakes).


To build `mlsus` from source, follow these steps:
```sh
# Clone the repository
git clone https://github.com/johnyob/mlsus.git
# Enter the Nix development environment
nix develop
# Build 🚀
dune build
```

## License 

This project is licensed under the GNU GPL v3.0 license.
