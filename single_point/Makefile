
include ../user_build_config

OBJS =	module_time_utilities.o \
	create_point_data.o
	
all:	$(OBJS)
	$(COMPILERF90) -I. $(NETCDFMOD) -o create_point_data.exe $(OBJS) $(NETCDFLIB)

module_time_utilities.o: ../util/module_time_utilities.f90
	$(COMPILERF90) -c ../util/module_time_utilities.f90

create_point_data.o: create_point_data.f90
	$(COMPILERF90) -I. $(NETCDFMOD) -c create_point_data.f90 $(NETCDFLIB)

clean:
	rm -f *.o *.mod *.exe

#
# Dependencies:
#

create_point_data.o:	module_time_utilities.o
