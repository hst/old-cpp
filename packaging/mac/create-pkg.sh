#!/bin/bash

MAJOR_VERSION="1.0"
VERSION="1.0-α3"

PACKAGING_DIR=$PWD
ROOT_DIR=$PWD/../..

FRAMEWORK_DIR=$PWD/HST.framework
VERSION_DIR=${FRAMEWORK_DIR}/Versions/${MAJOR_VERSION}
RESOURCES_DIR=${VERSION_DIR}/Resources
USR_DIR=${VERSION_DIR}/usr


# Remove any existing bundle.

if [ -x ${FRAMEWORK_DIR} ]; then
    echo Removing old framework bundle...
    rm -rf ${FRAMEWORK_DIR}
fi

if [ -x HST.mpkg ]; then
    echo Removing old installation package...
    rm -rf HST.mpkg
fi


# Set up the versioned framework bundle.

echo Creating HST framework in ${FRAMEWORK_DIR}...

mkdir -p ${VERSION_DIR}
ln -s ${MAJOR_VERSION} HST.framework/Versions/Current


# Create the Resources directory and copy in the Info.plist file.

mkdir -p ${RESOURCES_DIR}
ln -s Versions/Current/Resources ${FRAMEWORK_DIR}/
cp Info.plist ${RESOURCES_DIR}


# Build the C++ code, installing into $USR_DIR.

echo ""
echo Configuring C++ code...

mkdir -p build-c++
cd build-c++

cmake \
    -D CMAKE_BUILD_TYPE:string=RELEASE \
    -D CMAKE_INSTALL_PREFIX:string=${USR_DIR} \
    ${ROOT_DIR}

echo ""
echo Building C++ mode...

make

echo ""
echo Installing C++ code into bundle...

make install
ln -s Versions/Current/usr/lib/libhst.dylib ${FRAMEWORK_DIR}/
ln -s Versions/Current/usr/bin/csp0 ${FRAMEWORK_DIR}/

cd ${PACKAGING_DIR}


# Copy Judy library into the bundle.

# In this regexp, the first [^ ] is contains a tab character; the
# second contains a space.

OLD_JUDY_PATH=$(otool -L ${FRAMEWORK_DIR}/libhst.dylib |
    grep -E -o "[^	]+Judy[^ ]+")

echo ""
echo Copying ${OLD_JUDY_PATH} into bundle...

cp ${OLD_JUDY_PATH} ${USR_DIR}/lib/

JUDY_NAME=$(basename ${OLD_JUDY_PATH})


# Build the Haskell library.

HASKELL_DIR=${ROOT_DIR}/cspm

echo ""
echo Configuring Haskell code...

cd ${HASKELL_DIR}
ghc --make Setup.lhs

./Setup configure

echo ""
echo Building Haskell code...

./Setup build

echo ""
echo Installing Haskell code into bundle...

cp dist/build/cspm/cspm ${USR_DIR}/bin/
ln -s Versions/Current/usr/bin/cspm ${FRAMEWORK_DIR}/

cd ${PACKAGING_DIR}


# We'll eventually place the framework into /Libraries/Frameworks.
# Change the link ID of the two libraries in the bundle to reflect
# this, and make sure that all of the link pointers are set up
# correctly.

echo ""
echo Fixing DYLD link pointers...

echo "  ${JUDY_NAME}"

OLD_JUDY_ID=$(otool -D ${USR_DIR}/lib/${JUDY_NAME} | tail -n 1)
JUDY_ID=/Libraries/Frameworks/HST.framework/Version/${MAJOR_VERSION}/usr/lib/${JUDY_NAME}

install_name_tool -id ${JUDY_ID} ${USR_DIR}/lib/${JUDY_NAME}

echo "  libhst.dylib"

OLD_LIBHST_ID=$(otool -D ${USR_DIR}/lib/libhst.dylib | tail -n 1)
LIBHST_ID=/Libraries/Frameworks/HST.framework/Version/${MAJOR_VERSION}/usr/lib/${OLD_LIBHST_ID}

install_name_tool -id ${LIBHST_ID} ${USR_DIR}/lib/libhst.dylib

install_name_tool -change ${OLD_JUDY_PATH} ${JUDY_ID} ${USR_DIR}/lib/libhst.dylib

echo "  csp0"

install_name_tool -change ${OLD_JUDY_PATH} ${JUDY_ID} ${USR_DIR}/bin/csp0
install_name_tool -change ${OLD_LIBHST_ID} ${LIBHST_ID} ${USR_DIR}/bin/csp0


# And finally, build the installation package.

echo ""
echo "Building installation package..."

/Developer/usr/bin/packagemaker --doc HST.pmdoc -o HST.mpkg