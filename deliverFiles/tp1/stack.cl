class StackElement { -- Class that represents a stack element, containing the content and the pointer to the next stack element
    content: String;
    nextStackElement: StackElement;


    setContent (elem: String) : Object {
        content <- elem
    };

    getContent() : String {
        content
    };

    getNextstackElement() : StackElement {
        nextStackElement
    };

    pushNextElementToStack(elem: StackElement) : Object {
        nextStackElement <- elem
    };
};

class Stack inherits IO { -- Class that implements basic operations in stack (push and pop)
    top: StackElement;
    bottom: StackElement;
    newStackElement: StackElement;
    elementContent: String;
    currentElement: StackElement;


    push(str: String) : Object {{ -- Method that implements the push basic stack operation
        newStackElement <- new StackElement;
        newStackElement.setContent(str);
        if isvoid top then { -- If there is not top element
            top <- newStackElement;
        }

        else { -- If there is a top element
            newStackElement.pushNextElementToStack(top);
            top <- newStackElement;
        }
        fi;
    }};

    pop() : String {{ -- Method that implements the pop basic stack operation
        elementContent <- top.getContent();
        top <- top.getNextstackElement();
        elementContent;
    }};
};

class StackCommands inherits Stack { -- Class that implements all commands for this program and also inherits basic stack commands from Stack class
    charIntegerConverter: A2I; -- Gets the A2I library object
    topStackElement1: String;
    topStackElement2: String;
    sum: Int;


    exchange() : Object {{ -- Exchange the two top items in the stack
        topStackElement1 <- pop();
        topStackElement2 <- pop();
            
        push(topStackElement1);
        push(topStackElement2);
    }};

    printStack() : Object {{ -- Prints the entire stack at the current state
        currentElement <- top;
        
        while not isvoid currentElement loop { -- While the current content is not void
            out_string(currentElement.getContent()); -- Prints the current content content
            out_string("\n");
            currentElement <- currentElement.getNextstackElement(); -- Gets the next stack content
        }
        pool;
    }};

    sum() : Object {{ -- Sums the two top stack elements, converting them to integers, converting them back to string and pushing to the stack
        charIntegerConverter <- new A2I;
        topStackElement1 <- pop();
        topStackElement2 <- pop();

        sum <- charIntegerConverter.a2i(topStackElement1) + charIntegerConverter.a2i(topStackElement2);

        push(charIntegerConverter.i2a(sum));
    }};
};

class Main inherits IO {
    stackCommands: StackCommands;
    running: Bool;
    input: String;
    topStackCommand: String;


    main() : Object {{
        stackCommands <- new StackCommands; -- Creates a new StackCommands object to implements all needed attributes and methods
        running <- true; -- Sets the running flag to true

        while running loop { -- While the running flag is true
            out_string("> ");
            input <- in_string(); -- Gets the input from the user

            if input = "x" then { -- If the input command is exit
                running <- false; -- Sets the running flag to false
            }

            else {
                if input = "e" then { -- If the input command is execute
                    topStackCommand <- stackCommands.pop(); -- Pops the top command of the stack

                    if topStackCommand = "s" then { -- If the top command is exchange
                        stackCommands.exchange(); -- Calls the exchange method
                    }
                    
                    else {
                        if topStackCommand = "+" then { -- If the top command is sum
                            stackCommands.sum(); -- Calls the sum method
                        }
                        else {
                            stackCommands.push(topStackCommand); -- If the top command is not possible to execute, then push it to the stack again
                        }
                        fi;
                    }
                    fi;
                }

                else { -- If the input is not execute
                    if input = "d" then { -- If the input is d (print stack)
                        stackCommands.printStack(); -- Calls the printStack method
                    }
                    
                    else { -- If the input is not execute and neither print, then it may be a integer
                        stackCommands.push(input); -- Calls the method to push the input
                    }
                    fi;
                }
                fi;
            }
            fi;
        }
        pool;
    }};
};
