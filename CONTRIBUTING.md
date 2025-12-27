# Contributing to LearnTidyML

Thank you for your interest in contributing to LearnTidyML! This document provides guidelines for contributing to the project.

## How to Contribute

### Reporting Issues

If you find a bug or have a feature suggestion:

1. **Check existing issues** - Search the [issue tracker](https://github.com/jasongeslois/LearnTidyML/issues) to see if it's already reported
2. **Create a new issue** - If not found, open a new issue with:
   - A clear, descriptive title
   - Steps to reproduce (for bugs)
   - Expected vs. actual behavior
   - R version and operating system
   - Relevant error messages or screenshots

### Suggesting Enhancements

We welcome ideas for improving LearnTidyML! When suggesting enhancements:

- Explain the use case and why it would benefit users
- Consider the educational focus of the package
- Be specific about the proposed behavior

### Pull Requests

1. **Fork the repository** and create your branch from `main`
2. **Make your changes** following the coding standards below
3. **Test your changes** - Run `devtools::check()` to ensure no errors
4. **Update documentation** if needed (roxygen2 comments, README, etc.)
5. **Submit a pull request** with a clear description of changes

## Development Setup

```r
# Clone and set up development environment
git clone https://github.com/jasongeslois/LearnTidyML.git
cd LearnTidyML

# Install dependencies
devtools::install_deps(dependencies = TRUE)

# Load package for development
devtools::load_all()

# Run tests
devtools::test()

# Check package
devtools::check()
```

## Coding Standards

### R Code Style

- Follow the [tidyverse style guide](https://style.tidyverse.org/)
- Use meaningful variable and function names
- Add roxygen2 documentation for exported functions
- Keep functions focused and modular

### Commit Messages

- Use clear, descriptive commit messages
- Start with a verb (Add, Fix, Update, Remove, etc.)
- Reference issue numbers when applicable (e.g., "Fix #123: ...")

### Documentation

- Update roxygen2 comments for any function changes
- Run `devtools::document()` to regenerate documentation
- Update README.md for user-facing changes
- Add entries to NEWS.md for significant changes

## Testing

- Add tests for new functionality in `tests/testthat/`
- Ensure all existing tests pass
- Aim for meaningful test coverage

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-your-feature.R")
```

## Questions?

If you have questions about contributing, feel free to:
- Open an issue with your question
- Contact the maintainer at jasongeslois@outlook.com

## Code of Conduct

Please note that this project is released with a [Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

---

Thank you for helping make LearnTidyML better!
