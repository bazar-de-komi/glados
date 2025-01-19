
# Glados

## Table of Contents

- [Project Overview](#project-overview)
- [Authors](#authors)
- [Repository](#repository)
- [Project Setup](#project-setup)
- [License](#license)
- [Commit norm](#commit-norm)
- [Acknowledgments](#acknowledgments)
- [References](#references)

---

## Project Overview

**Glados** (repository name: `glados`) is a program designed to compile a customed language. The project includes:

- A [parser](src/Parser/Parser.md) that parses every instruction of our language
- A compiler that convert it into [bytecode](src/Compiler/Bytecode.md).
- A [virtual machine](src/VM/VM.md) which acts as an executable

Our language is named Kleftis, and is inspired by Python, Haskell, and innovative ideas.

---

## Authors

This project is authored by:

- **Edouard Bocquet**
- **Eric Xu**
- **Flavien Maillard**
- **Harleen Singh-Kaur**
- **Thomas Lebouc**

Ownership belongs to **Epitech**, with development conducted between **November** and **January**.

---

## Repository

The source code and version control for the project can be found at the following GitHub repository:

- [Glados GitHub Repository](https://github.com/bazar-de-komi/glados)

---

## Project Setup

### Prerequisites

To set up the project locally, ensure you have the following tools installed:

- **Git** (or at least a local version of the repository): This will be used as the source for the tutorial
- **ghci**: This will be used to create glados' executable

### Usage

If you already have an instance of the repository on your computer you can skip step 1.

1. Clone the repository to your local machine:

    ```bash
    git clone https://github.com/bazar-de-komi/glados.git
    cd glados
    ```

2. <Presenting our language... so we need to make it obviously>

3. Create Glados executable:

    ```bash
    make
    ```

4. Compile file:

    ```bash
    ./glados <file.scm> || nothing
    input scm code
    ```

5. Run program (part 2 of the project):

### Automated testing

In order to run the tests for this projects we use the Makefile to run unit tests and functional tests:

launch all test (unit and functional)
```bash
make test
```
launch unit test
```bash
make test-unit
```
launch functional test
```bash
make test-functional
```
launch coverage
```bash
make test-coverage
```

---

## License

This project is under the ownership of **[Epitech](https://epitech.eu)** all rights reserved

---

## Commit norm

You can find the norm used for this repository here:

- [Commit norm](./COMMIT_CONVENTION.md)

---

## Acknowledgments

Thank you to the following people that have made this project possible:

Main contributors:

- **[Edouard Bocquet](https://github.com/edouardclm)**
- **[Eric Xu](https://github.com/KomiWolf)**
- **[Flavien Maillard](https://github.com/flavienepitech)**
- **[Harleen Singh-Kaur](https://github.com/Harleen-sk)**
- **[Thomas Lebouc](https://github.com/OrionPX4k)**

Everybody else:

[![All the contributors of glados.](https://contrib.rocks/image?repo=bazar-de-komi/glados)](https://contrib.rocks/image?repo=bazar-de-komi/glados)

---

## References

- [Epitech](https://epitech.eu)
- [Epitech GitHub](https://github.com/epitech)
