SRCS				= 	srcs/ft_turing.hs \

SRCS_MULTI			= 	srcs/ft_multi_turing.hs \

OBJS1				= ${SRCS:.hs=.o} ${SRCS_MULTI:.hs=.o}
OBJS2				= ${SRCS:.hs=.hi} ${SRCS_MULTI:.hs=.hi}

NAME				= ft_turing

NAME_MULTI			= ft_multi_turing

FLAGS				= -Wall -Wextra -Werror

all :				
					ghc -o ${NAME} ${SRCS}

multi :				
					ghc -o ${NAME_MULTI} ${SRCS_MULTI}

clean :				
					rm -rf ${OBJS1} ${OBJS2}

fclean : 			clean
					rm -rf ${NAME} ${NAME_MULTI}

re :				fclean all

.PHONY:				all clean fclean re