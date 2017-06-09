#!/usr/bin/env python
import re
import sys
from mpl_toolkits.axes_grid.axislines import SubplotZero
import matplotlib.pyplot as plt
import numpy as np


def plot_func(a, b, c, out_putline):
    '''Plot the line function in a coordinate axis

        Parameters:
        a -- The coefficient for y in the standard for Ay + Bx = C
        b -- The coefficient for x in the standard for Ay + Bx = C
        c -- The constanct C in the standard for Ay + Bx = C
        out_putline -- The string of the standard form Ay + Bx = C
    '''

    # Unable to plot if the coefficient of x or y equals to 0
    if b == 0:
        sys.stderr.write('Unable to plot a vertical line!!!\n')
        sys.exit(1)

    if a == 0:
        sys.stderr.write('Unable to plot a horizontal line!!!\n')
        sys.exit(1)

    # General steps to generate an axis
    try:
        fig = plt.figure(1)
        ax = SubplotZero(fig, 111)
        fig.add_subplot(ax)
    except Exception, e:
        sys.stderr.write('Unable to use matplotlib!!!\n')
        sys.exit(1)

    for direction in ["xzero", "yzero"]:
        ax.axis[direction].set_axisline_style("-|>")
        ax.axis[direction].set_visible(True)

    for direction in ["left", "right", "bottom", "top"]:
        ax.axis[direction].set_visible(False)

    # Need to use the format y = Kx + B
    # Get coefficient K and constant B
    K = -a / b
    B = c / b

    # Generate a proper range for the axis
    x_range = np.absolute(B/K * 3.0)
    if x_range != 0:
        x = np.linspace(0-x_range, x_range, 100)
    else:
        x = np.linspace(-10, 10, 100)

    # Plot
    ax.plot(x, K*x+B)
    plt.title(out_putline, fontsize=12, loc='left')
    plt.show()


def coordinate_matcher(coordinate):
    '''Match if the input is a coordinate

        Parameter:
        coordinate -- The input coordinate pair by users
    '''

    # Match the input by using regular expression
    pattern = re.compile('\s*[-]?\d+([.]\d+)?\s*,\s*[-]?\d+([.]\d+)?\s*')
    match = pattern.match(coordinate)
    if match:
        return True
    else:
        sys.stderr.write('Invalid input!!!')
        sys.stderr.write('The input should follow the format:number,number \n')
        sys.stderr.write('For example: 3,4 \n')
        return False


def get_output_line(x1, y1, x2, y2):
    '''Calculate the coefficient of the standard form Ax + By + C

        Parameters:
        x1, y1, x2, y2 -- The coordinates (x1, y1), (x2, y2)
    '''

    # Invalid if the two coordinates are same
    if((x1 == x2) and (y1 == y2)):
        sys.stderr.write('Invalid coordinates!!!\n')
        sys.exit(1)

    a = y1 - y2
    b = x2 - x1
    c = x2 * y1 - x1 * y2

    return (a, b, c)


def main():
    # Display help message
    sys.stderr.write('The input should follow the format:number,number \n')
    sys.stderr.write('For example: 3,4 \n')

    # Keep asking users to input until the coordinate is valid
    while True:
        print("Please input first coordinate:")
        coordinate_1 = sys.stdin.readline()
        if coordinate_matcher(coordinate_1):
            break

    while True:
        print("Please input second coordinate:")
        coordinate_2 = sys.stdin.readline()
        if coordinate_matcher(coordinate_2):
            break

    # Extract float number from the input string
    coor_1 = coordinate_1.split(',')
    coor_2 = coordinate_2.split(',')
    try:
        x1 = float(coor_1[0])
        y1 = float(coor_1[1])

        x2 = float(coor_2[0])
        y2 = float(coor_2[1])
    except Exception, e:
        sys.stderr.write('Invalid coordinates!!!\n')
        sys.exit(1)

    # Call functions to calculate the result and plot it
    a, b, c = get_output_line(x1, y1, x2, y2)
    out_putline = "%fx%sy=%f\n" % (a, ('+'+str(b) if b >= 0 else str(b)), c)
    sys.stdout.write('result:\n' + out_putline)
    plot_func(a, b, c, out_putline)


if __name__ == "__main__":
    main()
