.PHONY: all clean run test submit
all:
	./all-compile.sh

submit:
	./make-submit.sh

run:
	scala Main < testcase/example1.in 

test:
	scala Main < ../testcase.in > ../result.out
	../scripts/score_evaluator.out ../testcase.in ../result.out

clean:
	${RM} *.class
