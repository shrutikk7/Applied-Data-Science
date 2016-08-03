#Shrutik Katchhi 
#IST687 - HW 1: Boolean Logic

# Computers are known to carry out computations, and to carry out those computations they need to know how to do basic math.
# They do not add and subtract numbers as we do. They use microprocessors to add and subtract numbers.
# These microprocessors are made up of many ports and contain wires which help them to add and subtract numbers.
# Generally, the are which computes these calculations are made up of transistors.
# A transistor is like a machine, which can sometime conduct electric current and sometimes not.
# Imagine that we fix a light bulb at the end of the transistor, so if we have a switch, if we turn it ON and the current will flow through the transistor and if we switch it OFF then no current.

# Now, let's just remember that computers operate in 1s and 0s, so logically we express ON as 1 and OFF as 0, also 1 is TRUE and 0 is FALSE.


# OR gate:
# Imagine if we have two transistirs connecter to each other, each of them have their own switch. And we have one bulb which is connected to both transistors.
# So, of we switch ON any one of the transistors, the bulb will turn on.

if (1 | 1) print("1 OR 1: TRUE") else print ("1 OR 1: FALSE")
if (1 | 0) print("1 OR 0: TRUE") else print ("1 OR 0: FALSE")
if (0 | 1) print("0 OR 1: TRUE") else print ("0 OR 1: FALSE")
if (0 | 0) print("0 OR 0: TRUE") else print ("0 OR 0: FALSE")

# As we can see in the statements above, if both the switches are OFF (0|0) then we have a condition false.

# AND gate:
# Let's consider the same setuo as the OR gate, and the only thing which we change is the connection of the bulb, here, we connect the bulb to just one transictor.
# Hence, in order for the bulb to glow, both the switches have to be ON.


if (1 & 1) print("1 OR 1: TRUE") else print ("1 OR 1: FALSE")
if (1 & 0) print("1 OR 0: TRUE") else print ("1 OR 0: FALSE")
if (0 & 1) print("0 OR 1: TRUE") else print ("0 OR 1: FALSE")
if (0 & 0) print("0 OR 0: TRUE") else print ("0 OR 0: FALSE")

# NOT gate:
# This gate just reverses the output:

if (!1) print ("Not 1 is: TRUE") else print ("Not 1 is: FALSE")
if (!0) print ("Not 0 is: TRUE") else print ("Not 0 is: FALSE")

# These are the basics of boolean algebra, we use AND, OR and NOT gates in various combinations to get he desired output, bit every little bit of computations comes down to these basic gates.



# Reference: https://www.youtube.com/watch?v=VBDoT8o4q00