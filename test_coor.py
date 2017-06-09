import pytest
import coor
import random


@pytest.mark.parametrize("test_input, expected_output",
                         [
                            ('2,5', True),
                            ('-2,5', True),
                            ('2,-5', True),
                            ('-2,-5', True),
                            (' 2.375,5.897 ', True),
                            ('-2.908, -5.782', True),
                            ('  -789230.9054234,  5480495.7203948458  ', True),

                            ('-324890', False),
                            ('324890,', False),
                            (',324890', False),
                            ('324890..23,34.4', False),
                            ('asd,sdf', False)
                         ])
def test_coordinate_matcher(test_input, expected_output):
    '''Test input format of user input'''

    result = coor.coordinate_matcher(test_input)
    assert result == expected_output


def almost_equal(x, y, threshold=0.0000001):
    '''Since the percision of float in python is limited
        Define almost_equal to fix the inaccuracy  of percision
    '''

    return abs(x - y) < threshold


@pytest.mark.parametrize("test_input",
                         [
                            ([1, 2, 5, 6]),

                            ([0.8, -2.9, -0.234, 65.332]),

                            (
                                random.randint(-10000, 10000),
                                random.randint(-10000, 10000),
                                random.randint(-10000, 10000),
                                random.randint(-10000, 10000)
                            ),

                            (
                                random.randint(-10000, 10000),
                                random.randint(-10000, 10000),
                                random.randint(-10000, 10000),
                                random.randint(-10000, 10000)
                            ),

                            (
                                random.randint(-10000, 10000),
                                random.randint(-10000, 10000),
                                random.randint(-10000, 10000),
                                random.randint(-10000, 10000)
                            ),

                            (
                                random.randint(-10000, 10000),
                                random.randint(-10000, 10000),
                                random.randint(-10000, 10000),
                                random.randint(-10000, 10000)
                            ),

                            (
                                random.uniform(-10000.0, 10000.0),
                                random.uniform(-10000.0, 10000.0),
                                random.uniform(-10000.0, 10000.0),
                                random.uniform(-10000.0, 10000.0)
                            ),

                            (
                                random.uniform(-10000.0, 10000.0),
                                random.uniform(-10000.0, 10000.0),
                                random.uniform(-10000.0, 10000.0),
                                random.uniform(-10000.0, 10000.0)
                            ),

                            (
                                random.uniform(-10000.0, 10000.0),
                                random.uniform(-10000.0, 10000.0),
                                random.uniform(-10000.0, 10000.0),
                                random.uniform(-10000.0, 10000.0)
                            ),

                            (
                                random.uniform(-10000.0, 10000.0),
                                random.uniform(-10000.0, 10000.0),
                                random.uniform(-10000.0, 10000.0),
                                random.uniform(-10000.0, 10000.0)
                            ),
                         ])
def test_get_output_line(test_input):
    '''Test the correctness of the output line function
        by taking the coordinates into the line function
    '''

    x1 = test_input[0]
    y1 = test_input[1]
    x2 = test_input[2]
    y2 = test_input[3]

    a, b, c = coor.get_output_line(x1, y1, x2, y2)
    assert almost_equal(a * x1 + b * y1, c) and \
        almost_equal(a * x2 + b * y2, c)
