# GNU makefile for mrc, the resource compiler

.PHONY: firstTarget
firstTarget: all

include make.config

PACKAGES	= 
WARNINGS	= all no-multichar no-unknown-pragmas no-deprecated-declarations

RANLIB		?= ranlib
GIT			?= git
PROCESSOR	?= $(shell uname -p)
VERSION		?= $(shell $(GIT) describe)-$(PROCESSOR)

DEFINES				+= VERSION='"$(VERSION)"'
DEFINES				+= PDB_DIR='"$(PDB_DIR)"'
DEFINES				+= PDB_REDO_DIR='"$(PDB_REDO_DIR)"'

CFLAGS				+= -std=c++14
CFLAGS				+= $(addprefix -I, $(INCLUDE_DIR))

BOOST_LIBS			= system filesystem program_options
BOOST_LIBS			:= $(BOOST_LIBS:%=boost_%$(BOOST_LIB_SUFFIX))
LIBS				+= z $(BOOST_LIBS)

# generic defines

CFLAGS				+= $(addprefix -W, $(WARNINGS))
CFLAGS				+= $(addprefix -D, $(DEFINES))

ifneq ($(PACKAGES),)
CFLAGS				+= $(shell PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) pkg-config --cflags $(PACKAGES))
LDFLAGS				+= $(shell PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) pkg-config --libs $(PACKAGES))
endif
LDFLAGS				+= $(LIBRARY_DIR:%=-L %) $(LIBS:%=-l%) -g

OBJDIR				= obj

ifneq ($(DEBUG),1)
CFLAGS				+= -O3 -DNDEBUG -g
else
CFLAGS				+= -g -DDEBUG 
OBJDIR				:= $(OBJDIR).dbg
endif

ifeq ($(PROFILE),1)
CFLAGS				+= -pg
LDFLAGS				+= -pg
OBJDIR				:= $(OBJDIR).profile
endif

SOURCES				= mrc.cpp

OBJECTS	= $(addprefix $(OBJDIR)/, $(notdir $(SOURCES:%.cpp=%.o)))
LIB_OBJECTS = $(addprefix $(OBJDIR)/, $(notdir $(LIB_SOURCES:%.cpp=%.o)))

SOURCE_DIRS	= $(sort $(dir $(SOURCES)))

CFLAGS				+= $(addprefix -I, $(SOURCE_DIRS))

empty = 
space = $(empty) $(empty)
join-with = 
SRC_VPATH = $(subst $(space),:,$(SOURCE_DIRS))

VPATH += $(SRC_VPATH)
VPATH += test-src

$(OBJDIR)/%.o: %.cpp | $(OBJDIR)
	@ echo ">>" $<
	@ $(CXX) -MD -c -o $@ $< $(CFLAGS) $(CXXFLAGS)

-include $(OBJECTS:%.o=%.d)

$(OBJECTS:.o=.d):

$(OBJDIR):
	@ test -d $@ || mkdir -p $@

.PHONY: clean all
clean:
	rm -rf $(OBJDIR)/* mrc

mrc-mini: $(OBJECTS) $(OBJDIR)/dummy.o
	$(CXX) -o $@ $^ $(LDFLAGS)

$(OBJDIR)/mrsrc.o: mrc-mini mrsrc.h
	./mrc-mini -o $@ $^

mrc: $(OBJECTS) $(OBJDIR)/mrsrc.o
	$(CXX) -o $@ $^ $(LDFLAGS) 

all: mrc

make.config:
	@ echo "Created empty make.config file"
	@ echo "" >> $@

