Experiment at your own risk.

THIS SOFTWARE IS NOT INTENDED FOR USE AS A PRIMARY FLIGHT INSTRUMENT
FOR USE IN THE OPERATION OF AIRCRAFT, OR FOR ANY USE WHERE THE
FAILURE OF THE SOFTWARE COULD LEAD TO ANY IMPAIRMENT OF PILOT
PERFORMANCE, CONTROL OR OPERATION OF THE AIRCRAFT.

The Aviation data is not current.

Two sets of programs are supplied.  One set for OS/2, NT, and later versions of 
Windows such as Windows 2000.  These are in the default destination directory.
The other set is for DOS or any system that can emulate a DOS system such as 
the MS Dos Prompt of Windows 95, 98, etc.  Simply copy over the DOS set from the
DOS subdirectory if your operating systems needs the older module format.
Both sets are Non graphical interfaces containing the same functionality.
    **** --> Use the correct set for your operating system.  ****
There is no installation requirements, registry settings needed.  Everything
is self contained within the directory structure you unzipped into.

ALSE6B is the main program.  All functions are accessible from it's main menu.

ALSE6BCF is a configuration subprogram.  It is called automatically from
ALSE6B to configure defaults and will call back to the main program.  Its 
purpose is to allow the definition of some printer control sequences and 
the various aircraft performance profiles.  

ALSE6BDB is a another subprogram called automatically to allow updates to 
the databases.  It is also called automatically from the main program as 
necessary when corresponding option is selected.

Settings are remembered in the file ALSE6B.CFG and it should not be edited
by any other program.  Use the ALSE6BCF program to change the settings.

AIRPLANE.DEF contains default characteristics of your aircraft.  Multiple
aircraft can be modeled by creating multiple DEF files, and simply switching
to them via the ALSE6BCF program.  One suggestion would be to use the aircraft's
N number as the name of the DEF file containing the characteristics of that
particular aircraft, such as weight and balance data points, etc.

The data files are named simply, NAVAIDS.DAT for VOR, NDB, and similar navaids.
FIXES.DAT stores intersections and waypoints.  The combination of AIRPORTS.DAT
and AIRPORTS.DTL store the base and detail airport information.

The source to the Basic program is provided "as is".  It is compiled under
a very old version of the IBM Basic Compiler.  It will not run under
interpretted basic as it uses very large arrays.

To Delete an individual Airport/Navaid/Fix, put a ~  (tilde) character as
the first character of the name.  That entry is marked for recycling and
can be resurrected by entering in that special name (with the ~) UNTIL
a new Airport/Navaid/Fix is added to the data bases.

Some text field contents can be deleted by specifying the ~ as the first
character.  The deleted field is not recoverable.

Generally, pressing ENTER on a menu all by itself returns you to the
previous menu.

Where a Navaid and an Airport have the same identifier, the Airport can
be entered into the Flight Plan by prefixing with a "K".  example, DFW
is the Dallas/Fort Worth VOR, but KDFW is the Dallas/Fort Worth airport.

Use at your own risk. This software is provides AS IS without warranty of any kind.

ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE EXPRESSLY
DISCLAIMED. THE AUTHOR DOES NOT WARRANT THAT THE FUNCTIONS CONTAINED IN THE SOFTWARE 
WILL MEET YOUR REQUIREMENTS, OR THAT THE OPERATION OF THE SOFTWARE WILL BE UNINTERRUPTED
OR ERROR-FREE, OR THAT DEFECTS IN THE SOFTWARE OR DATA WILL BE CORRECTED.
FURTHERMORE, THE AUTHOR DOES NOT WARRANT OR MAKE ANY REPRESENTATIONS REGARDING THE USE
OR THE RESULT OF THE USE OF THE SOFTWARE OR DATA OR RELATED DOCUMENTATION IN
TERMS OF THEIR CORRECTNESS, ACCURACY, RELIABILITY, OR OTHERWISE. NO ORAL OR WRITTEN
INFORMATION OR ADVICE GIVEN BY THE AUTHOR OR AN AUTHORIZED REPRESENTATIVE SHALL CREATE
A WARRANTY OR ANYWAY INCREASE THE SCOPE OF THIS WARRANTY. SHOULD THE SOFTWARE PROVE 
DEFECTIVE, YOU (AND NOT THE AUTHOR OR AN AUTHORIZED REPRESENTATIVE) ASSUME THE
ENTIRE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.

UNDER NO CIRCUMSTANCES INCLUDING NEGLIGENCE, SHALL THE AUTHOR BE
LIABLE FOR ANY INCIDENTAL, SPECIAL OR CONSEQUENTIAL DAMAGES THAT RESULT FROM THE USE
OR INABILITY TO USE THE SOFTWARE OR RELATED DOCUMENTATION, EVEN IF THE AUTHOR AN
AUTHORIZED REPRESENTATIVE HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.