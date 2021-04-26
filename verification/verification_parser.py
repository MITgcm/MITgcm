"""Parse verification outputs."""

import re
import glob
import os

def verification_parser(filename, threshold, input_dir_pat):

    # function should be given a threshold value for each sub test
    (directory, _) = os.path.split(filename)
    # how many additional tests are run with tweaks to this configuration
    num_exps = len(glob.glob(directory+input_dir_pat))+1

    # check that the correct number of values for `threshold` have been given
    if len(threshold) != num_exps:
        # some if statements to deal with grammar
        if len(threshold)==1:
            error_message = '{0} value given for threshold, '.format(len(threshold))
        else:
            error_message = '{0} values given for threshold, '.format(len(threshold))
        
        if num_exps==1:
            error_message = error_message + 'but {0} subtest found.'.format(num_exps)        
        else:
            error_message = error_message + 'but {0} subtests found.'.format(num_exps)

        raise ValueError(error_message)
        
    # open the testreport output to assess pass/fail            
    with open(filename) as f:
        lines = f.readlines()

        first_match = True

        # extract lines from output
        for i, line in enumerate(lines):
            if line[:5] == '2 d e':
                if first_match:
                    # skip the first match, since it doesn't contain output,
                    # but set to false to catch next matches.
                    first_match = False
                else:
                    # save the line number where the output is found
                    output_line = i
                    break

        # loop through each of the subexperiments:
        for j in xrange(len(threshold)):
            test_results = lines[output_line+2+j]

            # split test_results into a list with values for each number. 
            # this uses spaces and the < > characters to separate the numbers.
            test_results = re.split('[ ><]+',test_results)
            # Check the Genmake, depend, make, and run checks

            for status in test_results[:4]:
                assert status== 'Y'

            # Ignore the build status varaibles that were just checked, as
            # well as "pass" or "fail" and test name at the end of the line
            test_results = test_results[4:-2]

            # convert to floats
            dp_similarity = []
            for i, x in enumerate(test_results):
                try:
                    dp_similarity.append(float(x))
                except ValueError:
                    pass


            if len(dp_similarity) == 3:
                # adjoint test.
                # Remove forward gradient as it may have few matching digits.
                del dp_similarity[2]
            elif len(dp_similarity) >= 17:
                # this means that the test wasn't an offline advection test.
                # Remove the means of u and v since they are constrained 
                # to ~0 by domain geometry and can cause the test to fail 
                # when it shouldn't.
                del dp_similarity[15]
                del dp_similarity[11]

            assert all(elements >= threshold[j] for elements in dp_similarity)

if __name__ == '__main__':

    import argparse

    parser = argparse.ArgumentParser(description='Check that verification simulation passed the test.')

    parser.add_argument('-filename', type=str, 
                        help='path to output file from the verification test')

    parser.add_argument('-threshold',nargs='+', type=int, default=15, 
                        help='number of decimal places of similarity required for test to pass. Requires a value for each sub test. Separate values with a space.')

    parser.add_argument('-input_dir_pat', type=str, default='/input.*',
                        help='Directory pattern for searching for sub-experiments for base, oad, adm, tlm. Default /input.*')

    args = parser.parse_args()

    verification_parser(**vars(args))
