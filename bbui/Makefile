QMAKE_TARGET  = txtmpp
QMAKE         = $(QNX_HOST)/usr/bin/qmake
TARGET        = $(QMAKE_TARGET)
GHC_X86       = $(HOME)/src/ghc-qnx-nto-i486/inplace/bin/ghc-stage1 -O2 -XHaskell98 -Wall -fno-warn-name-shadowing -lsocket -DUIMODULE=HaskadesBinding
GHC_ARM       = $(HOME)/src/ghc-qnx-nto-arm/inplace/bin/ghc-stage1 -O2 -XHaskell98 -Wall -fno-warn-name-shadowing -lsocket -lcaps -DUIMODULE=HaskadesBinding
HASKADES      = haskades

all: Makefile $(QMAKE_TARGET)

Simulator-Debug.bar: Simulator-Debug
	blackberry-nativepackager -devMode -package $@ bar-descriptor.xml -configuration Simulator-Debug

Device-Debug.bar: Device-Debug
	blackberry-nativepackager -devMode -package $@ bar-descriptor.xml -configuration Device-Debug

Device-Release.bar: Device-Release
	blackberry-nativepackager -package $@ bar-descriptor.xml -configuration Device-Release

clean:
	$(MAKE) -C ./arm -f Makefile sureclean
	$(MAKE) -C ./x86 -f Makefile sureclean
	$(RM) ./src/HaskadesBinding.hs ./src/haskades_run.cpp
	$(RM) ./x86/o/.obj/*
	$(RM) ./x86/o-g/.obj/*
	$(RM) ./x86/o.le-v7/.obj/*
	$(RM) ./x86/o.le-v7-g/.obj/*

Makefile: FORCE src/haskades_run.cpp
	$(QMAKE) -spec unsupported/blackberry-armv7le-qcc -o arm/Makefile $(QMAKE_TARGET).pro CONFIG+=device
	$(QMAKE) -spec unsupported/blackberry-x86-qcc -o x86/Makefile $(QMAKE_TARGET).pro CONFIG+=simulator
	$(MAKE) -C ./translations -f Makefile update release

FORCE:

$(QMAKE_TARGET): device simulator

arm/HaskadesBinding.hs: src/HaskadesBinding.hsc src/haskades_run.h
	hsc2hs -o arm/HaskadesBinding.hs -c arm-unknown-nto-qnx8.0.0eabi-gcc -l arm-unknown-nto-qnx8.0.0eabi-gcc -I. -I src/ --cross-safe -x src/HaskadesBinding.hsc

x86/HaskadesBinding.hs: src/HaskadesBinding.hsc src/haskades_run.h
	hsc2hs -o x86/HaskadesBinding.hs -c i486-pc-nto-qnx8.0.0-gcc -l arm-unknown-nto-qnx8.0.0eabi-gcc -I. -I src/ --cross-safe -x src/HaskadesBinding.hsc

src/HaskadesBinding.hsc: ../Types.hs
	$(HASKADES) src/HaskadesBinding.hsc src/haskades_run.cpp src/haskades_run.h < ../Types.hs

src/haskades_run.cpp: ../Types.hs
	$(HASKADES) src/HaskadesBinding.hsc src/haskades_run.cpp src/haskades_run.h < ../Types.hs

src/haskades_run.h: ../Types.hs
	$(HASKADES) src/HaskadesBinding.hsc src/haskades_run.cpp src/haskades_run.h < ../Types.hs

device: src/haskades_run.cpp arm/HaskadesBinding.hs
	$(MAKE) -C ./arm -f Makefile all
	$(GHC_ARM) --make -threaded -i../ -i./arm/ -i./src/ -o ./arm/o.le-v7-g/Main -outputdir ./arm/o.le-v7-g/.obj/ -optl-Wl,-rpath-link="$(QNX_TARGET)/armle-v7/usr/lib/qt4/lib" -L./arm/o.le-v7-g/ -l$(QMAKE_TARGET) src/Main.hs
	$(GHC_ARM) --make -threaded -i../ -i./arm/ -i./src/ -o ./arm/o.le-v7/Main -outputdir ./arm/o.le-v7/ -optl-Wl,-rpath-link="$(QNX_TARGET)/armle-v7/usr/lib/qt4/lib" -L./arm/o.le-v7/ -l$(QMAKE_TARGET) src/Main.hs

Device-Debug: Makefile src/haskades_run.cpp arm/HaskadesBinding.hs
	$(MAKE) -C ./arm -f Makefile debug
	$(GHC_ARM) --make -threaded -i../ -i./arm/ -i./src/ -o ./arm/o.le-v7-g/Main -outputdir ./arm/o.le-v7-g/.obj/ -optl-Wl,-rpath-link="$(QNX_TARGET)/armle-v7/usr/lib/qt4/lib" -L./arm/o.le-v7-g/ -l$(QMAKE_TARGET) src/Main.hs

Device-Release: Makefile src/haskades_run.cpp arm/HaskadesBinding.hs
	$(MAKE) -C ./arm -f Makefile release
	$(GHC_ARM) --make -threaded -i../ -i./arm/ -i./src/ -o ./arm/o.le-v7/Main -outputdir ./arm/o.le-v7/ -optl-Wl,-rpath-link="$(QNX_TARGET)/armle-v7/usr/lib/qt4/lib" -L./arm/o.le-v7/ -l$(QMAKE_TARGET) src/Main.hs

simulator: src/haskades_run.cpp x86/HaskadesBinding.hs
	$(MAKE) -C ./x86 -f Makefile all
	$(GHC_X86) --make -threaded -i../ -i./x86/ -i./src/ -o ./x86/o/Main -outputdir ./x86/o/.obj/ -optl-Wl,-rpath-link="$(QNX_TARGET)/x86/usr/lib/qt4/lib" -L./x86/o/ -l$(QMAKE_TARGET) src/Main.hs
	$(GHC_X86) --make -threaded -i../ -i./x86/ -i./src/ -o ./x86/o-g/Main -outputdir ./x86/o-g/.obj/ -optl-Wl,-rpath-link="$(QNX_TARGET)/x86/usr/lib/qt4/lib" -L./x86/o-g/ -l$(QMAKE_TARGET) src/Main.hs

Simulator-Debug: Makefile src/haskades_run.cpp x86/HaskadesBinding.hs
	$(MAKE) -C ./x86 -f Makefile debug
	$(GHC_X86) --make -threaded -i../ -i./x86/ -i./src/ -o ./x86/o-g/Main -outputdir ./x86/o-g/.obj/ -optl-Wl,-rpath-link="$(QNX_TARGET)/x86/usr/lib/qt4/lib" -L./x86/o-g/ -l$(QMAKE_TARGET) src/Main.hs
