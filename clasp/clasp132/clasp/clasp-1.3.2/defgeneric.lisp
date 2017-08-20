;;;; -*- Mode:Common-Lisp; Package:CLASP; Base:10 -*-

;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; Copyright (c) 1990 - 1994 University of Massachusetts
;;; Department of Computer and Information Science
;;; Experimental Knowledge Systems Laboratory
;;; Professor Paul Cohen, Director.
;;; All rights reserved.
;;; 
;;; Permission to use, copy, modify and distribute this software and its
;;; documentation is hereby granted without fee for non-commercial uses only 
;;; (not for resale), provided that the above copyright notice of EKSL, this 
;;; paragraph and the one following appear in all copies and in supporting 
;;; documentation.
;;; EKSL makes no representation about the suitability of this software for any
;;; purposes.  It is provided "AS IS", without express or implied warranties
;;; including (but not limited to) all implied warranties of merchantability
;;; and fitness for a particular purpose, and notwithstanding any other
;;; provision contained herein.  In no event shall EKSL be liable for any
;;; special, indirect or consequential damages whatsoever resulting from loss
;;; of use, data or profits, whether in an action of contract, negligence or
;;; other tortuous action, arising out of or in connection with the use or
;;; performance of this software, even if EKSL is
;;; advised of the possiblity of such damages.
;;; 
;;; For more information write to clasp-support@cs.umass.edu
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(defgeneric variable-value (the-variable &key order-by where)
  (:documentation "Returns the value of `the-variable'.  If included,
`order-by' is the name of another variable which `the-variable' should
be sorted by, and `where' is a where-clause \(see ???\) selecting a
subset of the variable."))

;;vector-fns.lisp
#+ignore
(defun integer-sequence (&optional start end &key by))

#+ignore
(defun real-sequence (&optional start end &key number-of-points interval))

#+ignore
(defmacro dovector ((loop-variable vector &optional return-value) &body body))

#+ignore
(defun access (vector &rest args))

#+ignore
(defmacro do-sequence ((element-variable sequence &rest args)
		       &body body))

;;datasets.lisp
(defgeneric make-dataset (&key name description)
  (:documentation "Creates a new dataset and fills the `name' and
`description' slots.  Make-dataset also initializes a row-number
variable for the dataset."))

(defgeneric make-dataset-from-rows (name data column-names
				    &optional description)
  (:documentation "Creates a new data set and fills the `name' and
`description' slots.  Then it creates variables for the dataset using
`column-names' as the names of the variables and `data' as the data.
`Data' should be in row-major order."))

(defgeneric make-dataset-from-columns (name data column-names
				       &optional description)
  (:documentation "Creates a new data set and fills the `name' and
`description' slots.  Then it creates variables for the dataset using
`column-names' as the names of the variables and `data' as the data.
`Data' should be in column-major order."))

#+ignore
(defun rename-dataset (dataset new-name)
  "rename-dataset changes the name of `dataset to `new-name', insuring the
new name is unique.  `dataset' can either be a dataset or the name of a
dataset, and `new-name' must be a string.")

#+ignore
(defun delete-dataset (dataset)
  "Destroys a dataset.  `dataset' can either be a dataset or the name of a
dataset.")

#+ignore
(defun activate-dataset (dataset)
  "Puts a dataset on the clasp active-dataset list.")

(defgeneric deactivate-dataset (dataset)
  (:documentation "Takes a dataset off of the clasp active-datasets
list."))

(defgeneric get-dataset (the-dataset &optional active-datasets-only-p)
  (:documentation "Given a name or id, finds the dataset.  If
`active-datasets-only-p' is nil, will search through all current clasp
datasets, otherwise, only the active datasets are searched."))

#+ignore
(defun make-variable (&key name type attribute value
			   description dataset rtm-table)
  "make-variable creates a new variable.  `name' is a string.  If
`type' is one of :number, :symbol or :string, then that will be used
as the type, otherwise the type will be inferred from \(type-of
`type'\).  `value' is the data the variable should be initialized
with.  `description' is a description of the variable.  `dataset' is
the dataset to which the variable belongs.")

(defgeneric add-variable-to-dataset (dataset data variable-name
				     &optional description)
  (:documentation "add-variable-to-dataset adds the `data', which must
be a list of values, to `dataset' using `variable-name' as the name of
the new variable and `description' as a description."))

(defgeneric add-variables-to-dataset (dataset data-list variable-names
				      &optional descriptions)
  (:documentation "add-variables-to-dataset adds a number of variables
to `dataset'.  For each variable, i, its value \(which must be a
list\) is the ith element of `data-list', its name is the ith element
of `variable-names', and its description is the ith element of
descriptions."))

(defgeneric rename-variable (variable new-name)
  (:documentation "rename-variable changes the name of `variable' to
`new-name', insure the new name is unique.  `variable' can either be a
variable or the name of a variable, and `new-name' must be a string."))

(defgeneric delete-variable (variable)
  (:documentation "Destroys a variable.  `variable' can either be a
variable or the name of a variable."))

(defgeneric get-variable (the-variable
			  &optional dataset active-datasets-only-p)
  (:documentation "Returns a variable given the variable name.  If
variables in different datasets have the name being searched for, the
optional `dataset' argument should be used to indicate which
dataset\(s\) to look for the variable in.  The
`active-datasets-only-p' flag is only applicable when no `datasets'
variable is included.  It is used whether to describe all clasp
datasets currently loaded, or just those which are active."))

(defgeneric add-row-to-dataset (dataset data variables)
  (:documentation "This function will add `data' to the dataset
`dataset' as a new row.  `variables' is a list of variables and
specifies what order the new row is in with respect to the variables
in the dataset.  Therefore, `data' must match `variables' in both
length and the types of its values.

Note: Whenever this function is called, clear-cache should be called
on the same dataset."))

(defgeneric add-rows-to-dataset (dataset data-list variables)
  (:documentation "This function will add `data-list' to the dataset
`dataset'.  `data-list' is a list of lists representing rows of data.
The rows must be of equal length.  `variables' is a list of variables
and specifies what order the new rows are in with respect to the
variables in the dataset.  Therefore, each element of `data-list' must
match variables in both length and the types of its elements."))

;;translation-fns.lisp
#+ignore
(defun dataset-to-columns (data &key rows columns)
  "Takes a dataset `data' and extracts tuples representing the columns
of data in the dataset.  `rows' is a where-clause selecting which rows
of the dataset to select. `columns' is a list of variable names to
return.  By default, all rows and all columns are returned.")


#+ignore
(defun dataset-to-rows (data &key rows columns)
  "Takes a dataset `data' and extracts tuples representing the rows of
data in the dataset.  `rows' is a where-clause selecting which rows of
the dataset to select. `columns' is a list of variable names to
return.  By default, all rows and all columns are returned.")

#+ignore
(defun transpose (data)
  "Takes a list of lists, `data', and makes a list of tuples t where
t[i] contains all the i'th elements of the lists in `data'.")

;;manipulation-fns.lisp
#+ignore
(defun create-new-column-from-function (dataset expression)
  "`expression' must be a valid lisp expression, it may contain the
names of variables in `dataset'.  The rows of `dataset' are mapped
over, substituting the values of the variables in `expression', after
substitution, `expression' is evaluated.  This produces a new column
of data which is added as a variable to `dataset'.")

#+ignore
(defun recode (value old-values new-values)
  "Maps `value' from it's position in `original-values' to the value
at the equivalent position in `new-values'.  For instance,

\(recode 2 '\(1 2 3\) '\(a b c\)\) would return 'b.  This function can
be used with create-new-column-from-function to recode a categorical
variable.  From the program interface, it is recommended that
``recode-list'' be used instead.")

#+ignore
(defun recode-list (list old-values new-values)
  "Maps each element of `list' from it's position in `old-values' to
the value at the equivalent position in `new-values'.")

#+ignore
(defun partition-dataset (old-dataset partition-clause)

  "Often it is desirable to operate on a portion of a dataset, or to
separate out different parts of a dataset according to the values of
some of the variables.  When a dataset is broken up this way, it is
called partitioning, and the parts of the dataset created are called
partitions.  partition-dataset takes a dataset and a partition-clause
which describes how to split the dataset up, and returns a list of
datasets, each one containing one partition.  \(Note, this does not
destroy the original dataset.\)

Partition clauses are made up of boolean operators, and the
partitioning operator, ``.on.''.  The boolean operators act as
expected and are explained in detail in the reference manual.  The
.on. operator takes any number of variables as its operands, and
separates the dataset into different partitions for each unique set of
values of its operands.  For example, if a dataset consisted of a
boolean variable ``key1'', a categorical variable ``key2'', with
values 'a, 'b and 'c, and a data variable ``y'', and was partitioned
with a partitioning clause of \(.on. key1 key2\), then at most six new
datasets would be created, one containing all the rows of the original
dataset where key1 = true and key2 = 'a, one with all the rows where
key1 = false and key2 = 'a, one with all the rows where key1 = true
and key2 = 'b, and so on.  If a partition would be empty \(i.e. if
there were no rows where key1 = false and key2 = 'c\), then no dataset
is created for that partition, which is why AT MOST six new datasets
would be created in the above example.

The .on. operator can be used in conjunction with the boolean
operators.  When this happens, the .on. operator can be thought of as
a macro which causes a set of partitioning clauses to be created, each
exactly like the original, except that .on. is replaces with an .==.
operator and one of the unique values of the key.  If there are
multiple keys in the .on., it is replaced by \(.and. \(.==. key1
value11\) \(.==. key2 value21\), etc.\).  In the above example, a
partition clause of \(.and. \(.==. key1 true\) \(.on. key2\)\) would
cause up to three datasets to be created.  Each would contain rows
where key1 was true, and one would be created for key2 = 'a, one for
key2 = 'b and one for key2 = 'c.")

(defgeneric merge-datasets (t)
  (:documentation "Takes a list of source datasets and produces a new
dataset which merges the source datasets.  The variables in the new
dataset will be the union of all the variables in the source datasets,
plus an additional variable, SOURCE.  Each row in each source dataset
contributes one row to the new dataset.  That row will contain the
value in the source dataset for any variable which exists in the
source dataset, and nil for any others.  The source variable will
contain the name of the source dataset that contributed that row.  For
example, Given the following datasets:

 D1    V1 V2
        2  4
        5  1

 D2    V1 V3
        3  7
        9  6
        8  0

 \(merge '\(d1 d2\)\) will produce
 MERGE-OF-D1-D2    SOURCE  V1  V2  V3
                       D1   2   4 NIL
                       D1   5   1 NIL
                       D2   3 NIL   7
                       D2   9 NIL   6
                       D2   8 NIL   0"))

;; sampling-fns.lisp
(defgeneric sample-to-list (dist size-of-sample)
  (:documentation "Gets a sample from the distribution `dist' of size
`size-of-sample'"))

(defgeneric sample-to-dataset (dist size-of-samples
			       &optional number-of-samples dataset-name
					 variable-names)
  (:documentation "Gets a sample from the distribution `dist' of size
`size-of-sample' and creates a dataset out of it.  If
`number-of-samples' is > 1 then multiple samples are taken from the
distribution and each one forms one column of the dataset."))

(defgeneric deviate (dist)
  (:documentation "Return a single deviate from the distribution
`dist'"))

;;io-fns.lisp
#+ignore
(defun load-dataset (filename &optional (defaults *data-file-defaults*))
  "Reads a dataset from a file in standard CLASP format.  `filename' is
a string or pathname.  If `filename' does not exist, an error is
signaled. CLASP format files have two parts, a header and a data
section, the first line of the header is the name of the dataset,
represented as a string \(e.g. it must appear in double quotes.\)  The
following n lines each have one string with the name of one of the
variables in the dataset.  The rest of the file is the data section.
Each line in the data section is a lisp list containing one row of
data..  The order of the data in the rows is the same as the order of
the names of the variables in the header.  If a file is loaded more than
once, each successive load will return a new dataset.")

#+ignore
(defun import-dataset (filename &key (separator #\,) include-labels-p)
  "Reads a dataset in from a file, where `filename' is a string or
pathname.  If `filename' doesn't exist, an error is signaled.  The data
on each line in the file must be separated by `separator' characters or
whitespace.  No error checking is done to insure that each line of the
file contains the same number of data.  If `include-labels-p' is
non-nil, the first line will be used a variable names, otherwise they
will be named variable, variable-1, variable-2, etc.  The filename will
be used for the dataset name.")

#+ignore
(defun save-dataset (data filename)
  "Writes a dataset out to a file in CLASP format.  `data' must be a
dataset, and filename is a string or pathname of a file.  If `filename'
already exists, an error will be signaled.")

#+ignore
(defun export-dataset (dataset filename &key (separator #\,) include-labels-p)
  "Writes out the values in `dataset' in row-major order to the file
`filename.'  Each row in `filename' will be one row of data in
`dataset.' The data in a row will be separated by the character
`separator'.  If include-labels-p is non-nil, the variable-names will be
written out on the first line.  `export-dataset' will signal an error if
a file of name `filename' already exists.")
