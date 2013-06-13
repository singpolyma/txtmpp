QMAKE_TARGET  = txtmpp
QMAKE         = $(QNX_HOST)/usr/bin/qmake
TARGET        = $(QMAKE_TARGET)
GHC_X86       = $(HOME)/src/ghc-qnx-nto-i486/inplace/bin/ghc-stage1 -O2 -XHaskell98 -Wall -fno-warn-name-shadowing
GHC_ARM       = $(HOME)/src/ghc-qnx-nto-arm-unregistered-llvm/inplace/bin/ghc-stage1 -O2 -XHaskell98 -Wall -fno-warn-name-shadowing

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

src/HaskadesBinding.hs: src/Types.hs
	haskades src/HaskadesBinding.hs src/haskades_run.cpp < src/Types.hs

src/haskades_run.cpp: src/Types.hs
	haskades src/HaskadesBinding.hs src/haskades_run.cpp < src/Types.hs

device: src/haskades_run.cpp src/HaskadesBinding.hs
	$(MAKE) -C ./arm -f Makefile all
	$(GHC_ARM) --make -threaded -i./src/ -o ./arm/o.le-v7-g/Main -outputdir ./arm/o.le-v7-g/.obj/ src/Main.hs -optl-Wl,-rpath-link="$(QNX_TARGET)/armle-v7/usr/lib/qt4/lib" -L./arm/o.le-v7-g/ -l$(QMAKE_TARGET)
	$(GHC_ARM) --make -threaded -i./src/ -o ./arm/o.le-v7/Main -outputdir ./arm/o.le-v7/.obj/ src/Main.hs -optl-Wl,-rpath-link="$(QNX_TARGET)/armle-v7/usr/lib/qt4/lib" -L./arm/o.le-v7/ -l$(QMAKE_TARGET)

Device-Debug: Makefile src/haskades_run.cpp src/HaskadesBinding.hs
	$(MAKE) -C ./arm -f Makefile debug
	$(GHC_ARM) --make -threaded -i./src/ -o ./arm/o.le-v7-g/Main -outputdir ./arm/o.le-v7-g/.obj/ src/Main.hs -optl-Wl,-rpath-link="$(QNX_TARGET)/armle-v7/usr/lib/qt4/lib" -L./arm/o.le-v7-g/ -l$(QMAKE_TARGET)

Device-Release: Makefile src/haskades_run.cpp src/HaskadesBinding.hs
	$(MAKE) -C ./arm -f Makefile release
	$(GHC_ARM) --make -threaded -i./src/ -o ./arm/o.le-v7/Main -outputdir ./arm/o.le-v7/ src/Main.hs -optl-Wl,-rpath-link="$(QNX_TARGET)/armle-v7/usr/lib/qt4/lib" -L./arm/o.le-v7/ -l$(QMAKE_TARGET)

simulator: src/haskades_run.cpp src/HaskadesBinding.hs
	$(MAKE) -C ./x86 -f Makefile all
	$(GHC_X86) --make -threaded -i./src/ -o ./x86/o/Main -outputdir ./x86/o/.obj/ src/Main.hs -optl-Wl,-rpath-link="$(QNX_TARGET)/x86/usr/lib/qt4/lib" -L./x86/o/ -l$(QMAKE_TARGET)
	$(GHC_X86) --make -threaded -i./src/ -o ./x86/o-g/Main -outputdir ./x86/o-g/.obj/ src/Main.hs -optl-Wl,-rpath-link="$(QNX_TARGET)/x86/usr/lib/qt4/lib" -L./x86/o-g/ -l$(QMAKE_TARGET)

Simulator-Debug: Makefile src/haskades_run.cpp src/HaskadesBinding.hs
	$(MAKE) -C ./x86 -f Makefile debug
	$(GHC_X86) --make -threaded -i./src/ -o ./x86/o-g/Main -outputdir ./x86/o-g/.obj/ src/Main.hs -optl-Wl,-rpath-link="$(QNX_TARGET)/x86/usr/lib/qt4/lib" -L./x86/o-g/ -l$(QMAKE_TARGET)
