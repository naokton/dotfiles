#!/bin/bash

set -euo pipefail

WORK_DIR=$(dirname $0)
cd $WORK_DIR
rsync --files-from file_list.txt -arv ~/ .
