##
## EPITECH PROJECT, 2022
## Makefile
## File description:
## makefile
##

NAME		=	glados

NAMECABAL	=	glados.cabal

NAMETIX		=	glados-test.tix

PATHBIN 	=	$(shell stack path --local-install-root)

STACK		=	stack

COVERAGE_DIR	=	$(shell stack path --local-hpc-root)

all:	$(NAME)

$(NAME):
	$(STACK) build
	@cp $(PATHBIN)/bin/glados-exe ./$(NAME)

test:	clean
	stack test

test-coverage:	clean
	$(STACK) test --coverage
	@echo "Coverage report generated. Check HTML files in the coverage directory."
	@echo "Coverage directory:	$(COVERAGE_DIR)"

retest:	re	test

clean:
	@rm -f $(NAME)
	@rm -f $(NAMECABAL)
	@rm -f $(NAMETIX)
	@rm	-rf .stack-work
	@rm	-rf $(COVERAGE_DIR)

fclean:
	$(STACK) clean
	$(MAKE) clean

re:	fclean	all

docs:
	$(STACK) haddock

.PHONY:	re	all	clean	fclean	docs	test	retest	test-coverage
