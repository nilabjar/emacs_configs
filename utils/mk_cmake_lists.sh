#!/usr/bin/env bash

# The script will create CMakeLists.txt for a project.
#
# How to use the script.
# 1. cd a project directory
# 2. make_cmake_lst.sh
# 3. enter incudes directories not under subdirectories. The script will find all of subdirectories under the current directory.
# 4. just enter when you are done.
# 5. reload CMakeLists.txt from clion
# 6. the script will copy a previous CMakeLists.txt as a format of CMakeLists.txt.[timestamp]
#
# Example of Usage
# [hlee591@nymsgsdev1 workspace]$ ls
# hlee591          mockcpp  mrulesui  msgrules  msgssvc   output_dir  search-test
# basdemo-client  migration-tools  mrdusvc  msg       msgsmsvc  msgyenta  rule-test
# [hlee591@nymsgsdev1 workspace]$ cd msg
# [hlee591@nymsgsdev1 msg (svn/trunk)]$ 
# [hlee591@nymsgsdev1 msg (svn/trunk)]$ mk_cmake_lsts.sh 
# /* optional START: */
# -- Enter the directory of msg headers. Include offlinelib(msg headers) when you create non msg project
# Enter include directory not under subdirectories[just Enter when you are done.]: ~/mbig/workspace/msg/build/offlinelibs/00utputs/Linux-x86_64-64/big2017.02-859257-20170108T210802/include
# -- Enter the directory of bbit. The directory is required mainly for bbit_rich_text_body_limit.h
# Enter include directory not under subdirectories[just Enter when you are done.]: /bbsrc/thirdparty/bbit/include/bbit/201211
# -- Enter the directory of robo. The directory is required mainly for fastuuidtouser.h.
#    WARNING: If you include the robolibs directory, index creation will takes forever due to huge files.
#             I recommend to create a directory and copy only the header files you need, then enter the directory here.
# Enter include directory not under subdirectories[just Enter when you are done.]: ~/mbig/robo
#  >>> Or 
# Enter include directory not under subdirectories[just Enter when you are done.]: /bb/build/Linux-x86_64-64/release/robolibs/source/opt/bb/include
# /* optional END:  */
# -- Enter the directory of refroot
# Enter include directory not under subdirectories[just Enter when you are done.]: ~/mbig/refroot/refroot/amd64/opt/bb/include
# -- Enter the directory of mgp-api
# Enter include directory not under subdirectories[just Enter when you are done.]: ~/mbig/mgp-api/apicmn
# -- Enter to finish adding include directories.
# Enter include directory not under subdirectories[just Enter when you are done.]: 
#
# Previous CMakeLists.txt moved to CMakeLists.txt.1485812812.
#
# New CMakeLists.txt is created.
#

CUR_DIR=$(pwd | rev | cut -d '/' -f 1 | rev)
#FILES=$(find . -type f ! -empty -a -name '*.cpp' | grep -v '/build/' | grep -v '/lspbuild/' | grep -v cmake-build-debug | cut -d '/' -f 2-)
FILES=$(find . -type f ! -empty -a -name '*.cpp'  -not -path "*/build/*" -not -path "*/lspbuild/*" | grep -v '/build/' | grep -v '/lspbuild/' | grep -v cmake-build-debug | cut -d '/' -f 2-)
DIRS=$(echo "$FILES" | rev | cut -d '/' -f 2- | uniq | rev)
TIMESTAMP=$(date +%s)

INC_DIRS="SYSTEM
$PWD"

while read -r line; do 
  if [[ $line == *".cpp"* ]] ||  [[ $line == *".h"* ]]; then
    continue
  fi

  INC_DIRS+="
$PWD/$line"
done <<< "$DIRS"

while true
do 
  read -p "Enter include directory not under subdirectories[just Enter when you are done.]: " NEWPATH
  if [[ -z "$NEWPATH" ]]; then
    break;
  fi
  INC_DIRS="$INC_DIRS
$NEWPATH"
done
INC_DIRS+="
/bb/build/Linux-x86_64-64/release/robolibs/source/opt/bb/include
/home/nroy27/dpkg_refroot_code_index/refroot/amd64/opt/bb/include
/bbsrc/thirdparty/bbit/include
/bb/build/share/stp/include/00offlonly"


INC_DIRS="include_directories($INC_DIRS
)"

SRC_FILES="set(SOURCE_FILES
"
SRC_FILES+=$(echo "$FILES")
SRC_FILES+="
)"

ADD_EXE="add_executable($CUR_DIR \${SOURCE_FILES})"

TGT_INC_DIRS="target_include_directories($CUR_DIR PRIVATE \${PROJECT_SOURCE_DIR})"

TEMPLATE="cmake_minimum_required(VERSION 3.6)
project($CUR_DIR)

set(CMAKE_CXX_STANDARD 11)

$SRC_FILES

$ADD_EXE

$TGT_INC_DIRS

$INC_DIRS

add_definitions(-D_REENTRANT)
add_definitions(-D_THREAD_SAFE)
add_definitions(-D_SYS_SYSMACROS_H)
add_definitions(-DBB_THREADED)
add_definitions(-DBDE_BUILD_TARGET_DBG)
add_definitions(-DBDE_BUILD_TARGET_EXC)
add_definitions(-DBDE_BUILD_TARGET_MT)
add_definitions(-DBDE_OVERRIDES_STD)
add_definitions(-D_LINUX_SOURCE)

set(CMAKE_VERBOSE_MAKEFILE ON)"

if [ -f "CMakeLists.txt" ]; then
  OLD_FILE=CMakeLists.txt.${TIMESTAMP}
  mv CMakeLists.txt $OLD_FILE
  echo ""
  echo "Previous CMakeLists.txt moved to $OLD_FILE."
  echo ""
fi

echo -e "$TEMPLATE" > CMakeLists.txt
echo "New CMakeLists.txt is created."
