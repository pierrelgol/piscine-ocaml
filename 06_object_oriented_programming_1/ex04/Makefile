NAME = main
SRC = people.ml doctor.ml dalek.ml army.ml galifrey.ml main.ml

all: $(NAME)

$(NAME): $(SRC)
	ocamlc -o $(NAME) $(SRC)

clean:
	rm -f *.cmo *.cmi

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re
