%Created by Sri Vishnu Chadalavada
%Student ID: 1576993
%
%CSE 112 - Charlie McDowell
%
%Fall 19
%

%NOTE: EACH FLIGHT WILL HAVE AT LEAST A 30 MIN GAP BETWEEN ITS ARRIVAL AND THE NEXT DEPARTURE
%NOTE: IF THE DEPARTURE TIME IS BEFORE THE PREVIOUS ARRIVAL TIME IT IS ASSUMED THAT IT IS AT THAT TIME--
% --BUT THE NEXT DAY.

airport( atl, 'Atlanta         ', degmin(  33,39 ), degmin(  84,25 ) ).
airport( bos, 'Boston-Logan    ', degmin(  42,22 ), degmin(  71, 2 ) ).
airport( chi, 'Chicago         ', degmin(  42, 0 ), degmin(  87,53 ) ).
airport( den, 'Denver-Stapleton', degmin(  39,45 ), degmin( 104,52 ) ).
airport( dfw, 'Dallas-Ft.Worth ', degmin(  32,54 ), degmin(  97, 2 ) ).
airport( lax, 'Los Angeles     ', degmin(  33,57 ), degmin( 118,24 ) ).
airport( mia, 'Miami           ', degmin(  25,49 ), degmin(  80,17 ) ).
airport( nyc, 'New York City   ', degmin(  40,46 ), degmin(  73,59 ) ).
airport( sea, 'Seattle-Tacoma  ', degmin(  47,27 ), degmin( 122,17 ) ).
airport( sfo, 'San Francisco   ', degmin(  37,37 ), degmin( 122,23 ) ).
airport( sjc, 'San Jose        ', degmin(  37,22 ), degmin( 121,56 ) ).
flight( bos, nyc, time( 7,30 ) ).
flight( dfw, den, time( 8, 0 ) ).
flight( atl, lax, time( 8,30 ) ).
flight( chi, den, time( 8,45 ) ).
flight( mia, atl, time( 9, 0 ) ).
flight( sfo, lax, time( 9, 0 ) ).
flight( sea, den, time( 10, 0 ) ).
flight( nyc, chi, time( 11, 0 ) ).
flight( sea, lax, time( 11, 0 ) ).
flight( den, dfw, time( 11,15 ) ).
flight( sjc, lax, time( 11,15 ) ).
flight( atl, lax, time( 11,30 ) ).
flight( atl, mia, time( 11,30 ) ).
flight( chi, nyc, time( 12, 0 ) ).
flight( lax, atl, time( 12, 0 ) ).
flight( lax, sfo, time( 12, 0 ) ).
flight( lax, sjc, time( 12, 15 ) ).
flight( nyc, bos, time( 12,15 ) ).
flight( bos, nyc, time( 12,30 ) ).
flight( den, chi, time( 12,30 ) ).
flight( dfw, den, time( 12,30 ) ).
flight( mia, atl, time( 13, 0 ) ).
flight( sjc, lax, time( 13,15 ) ).
flight( lax, sea, time( 13,30 ) ).
flight( chi, den, time( 14, 0 ) ).
flight( lax, nyc, time( 14, 0 ) ).
flight( sfo, lax, time( 14, 0 ) ).
flight( atl, lax, time( 14,30 ) ).
flight( lax, atl, time( 15, 0 ) ).
flight( nyc, chi, time( 15, 0 ) ).
flight( nyc, lax, time( 15, 0 ) ).
flight( den, dfw, time( 15,15 ) ).
flight( lax, sjc, time( 15,30 ) ).
flight( chi, nyc, time( 18, 0 ) ).
flight( lax, atl, time( 18, 0 ) ).
flight( lax, sfo, time( 18, 0 ) ).
flight( nyc, bos, time( 18, 0 ) ).
flight( sfo, lax, time( 18, 0 ) ).
flight( sjc, lax, time( 18,15 ) ).
flight( atl, mia, time( 18,30 ) ).
flight( den, chi, time( 18,30 ) ).
flight( lax, sjc, time( 19,30 ) ).
flight( lax, sfo, time( 20, 0 ) ).
flight( lax, sea, time( 22,30 ) ).


/* haversine formula to get the distance from place to place taken by flights
 *
 * */
haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance ) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2 ) ** 2
      + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   Distance is Dist * 3961.

%end of math functions.

/*some get functions to make life easier */
get_name(In, Out) :- airport(In, Out, degmin( _X1, _Y1), degmin(_X2, _Y2)).

/*these give me both the degrees and the minutes*/
get_lat(In, Outdeg, Outmin) :-airport(In, _Name, degmin(Outdeg, Outmin), degmin(_X2, _Y2)).

get_long(In, Outdeg, Outmin) :-airport(In, _Name, degmin( _X1, _Y1), degmin(Outdeg, Outmin)).


/*this fucntion gives me the amount of time it would take to go from one airport to another */
flight_minutes(Start, End, OutTime) :-  
	/*Uses a bunch of gets to get all the necessary info */
	get_name(Start, _Name1),
	get_lat(Start, Latdeg1, _Latmin1),
	get_long(Start, Longdeg1, _Longmin1),
	get_name(End, _Name2),
	get_lat(End, Latdeg2, _Latmin2),
	get_long(End, Longdeg2, _Longmin2),
	
	/*Not sure why, but pi was assigned to a variable in haversine formula in mackeys code, so i did the same here */
	Pi is pi,

	/*converting from degrees to radians */
	Latrad1 is Latdeg1 * Pi / 180,
	Latrad2 is Latdeg2 * Pi / 180,
	Longrad1 is Longdeg1 * Pi / 180,
	Longrad2 is Longdeg2 * Pi / 180,

	/*now we have all the variables to plug into the haversine formula */	
	haversine_radians(Latrad1, Longrad1, Latrad2, Longrad2, Distance),
	/*We get the distance and the last thing we will do is divide by the spped of
 * 	the plane (500 miles per hour) and then multiply by 60 minutes to convert it
 * 	into minutes. It makes things easier to deal with when adding to the times
 * 	*/
	OutTime is Distance * 60/500.

/*These functions are meant to normalize the time so it isnt something weird like
 * 25:73 
 * Essentially, when you pass the values in itll keep subtracting 60 from the minutes
 * and will add 1 to the hours each time until the minutes are below 60 then itll
 * move on to the hours and keep subtracting 24 until it is below 24*/
normalizehours(Inhours, Inmin, Outhours, Outmin):- Inhours < 24,
	Outhours is Inhours,
	Outmin is Inmin,
	!.

normalizehours(Inhours, Inmin, Outhours, Outmin):- Inhours >= 24,
	Outhours0 is Inhours - 24,
	Outmin0 is Inmin,
	normalizehours(Outhours0, Outmin0, Outhours, Outmin),
	!.

normalizeminutes(Inhours, Inmin, Outhours, Outmin):- Inmin < 60,
        Outhours is Inhours,
        Outmin is round(Inmin),
        !.

normalizeminutes(Inhours, Inmin, Outhours, Outmin):- Inmin >= 60,
	Outhours0 is Inhours + 1,
	Outmin0 is Inmin - 60,
	normalizeminutes(Outhours0, Outmin0, Outhours, Outmin),
	!.
/*This function is supposed to add the travel time to the current time and normalize it */ 
add_airtime(Inhours, Inmin, Airmin, Outhours, Outmin) :- 
	
	Addedmin is Inmin + Airmin,
	normalizeminutes(Inhours, Addedmin, Outhours0, Outmin0),
	normalizehours(Outhours0, Outmin0, Outhours, Outmin).

/*Idk if this part is required but this is just print formatting so that numbers less than 10 in the minutes category are printed with a zero in front */
print_min(Min):- Min < 10,
	format('0~w ~n', [Min]),
	!.

print_min(Min):- Min >= 10,
	format('~w ~n', [Min]),
	!.

%a nice function that prints out the departure and arrival statements of a particular
%flight
depart_and_arrive(Start, End, H1, M1, H2, M2):- H1 =< H2, flight(Start, End, time(H1, M1)),
	get_name(Start, Sname),
	get_name(End, Ename),
	format('Depart: ~w ~w at ~w:', [Start, Sname, H1]),
	print_min(M1),
	format('Arrival: ~w ~w at ~w:', [End, Ename, H2]),
	print_min(M2),
	!.

%This checks for the neareest flight to a certain time and gives it in outhour and outmin
check_nearest_flight(Start, End, _Currhour, _Currminute, Outhour, Outmin):- flight(Start, End, time(Outhour, Outmin)).

  
%Neat packaging function that calls all the necessary individual functions
departure(Start, End, H1, M1, H2, M2):- flight(Start, End, time(H1,M1)),
	%gets the time length of the flight
	flight_minutes(Start, End, Airtime),
	%adds the length of the flight to the starting min and hours
	add_airtime(H1, M1, Airtime, H2, M2),
	%here it actually departs, and since it has the starting and end time it can
	%arrive too
	depart_and_arrive(Start, End, H1, M1, H2, M2).
	
%adds 30 minute to given hours, as that is the minimum for boarding time
add_boardtime(Inhours, Inmin, Outhours, Outmin) :-

        Addedmin is Inmin + 30.0,
        normalizeminutes(Inhours, Addedmin, Outhours0, Outmin0),
        normalizehours(Outhours0, Outmin0, Outhours, Outmin).

%code for helper functions	 
not( X ) :- X, !, fail.
not( _ ).

writepath( [] ) :-
   nl.
writepath( [Head|Tail] ) :-
   write( ' ' ), write( Head ), writepath( Tail ).

listpath( Node, End, Outlist ) :-
   listpath( Node, End, [Node], Outlist ).

%the only thing different is that I changed link to flight and inserted the appropriate vars
%This makes a list of all the flight paths I need while keeping a track of the visited nodes: Tried.
listpath( Node, Node, _, [Node]).
listpath( Node, End, Tried, [Node|List] ) :-
   	flight(Node, Next, time(_H1, _M1)),
   	not( member( Next, Tried )),
   	listpath( Next, End, [Next|Tried], List).

%end of code given by mackey

%gets first flight time
get_first_flight_time([Head | [Inner | _Tail]], Fhour, Fminute):-
       flight(Head, Inner, time(Fhour, Fminute)).

%takes list and out puts its head and tail, effectively popping it off, although since
%this is prolog you can't reassign it so i just out put it
popHead([Head | Tail], Head, Tail).	

%traverses the pth, uses the same structure as list path, kinda. It takes the start, location hour and minute as well as the path list.
traverse(_Start, _Hour, _Min, []).
traverse(Start, Hour, Min, [Head | Tail]):-
	%checks for nearest flight and gives it in H! and M!
	check_nearest_flight(Start, Head, Hour, Min, H1, M1),
	%Prints out the departure and adds the travel time to HoldH and HoldM
	departure(Start, Head, H1, M1, HoldH, HoldM),
	%then adds the boarding time, because boarding time is the minimum between flights
	add_boardtime(HoldH, HoldM, H2, M2),
	%passes it back through recursively
	traverse(Head, H2, M2, Tail).


fly(Start, Start):- /*If the Start and the End are the same this will fail */fail.

fly(Start, End):- flight(Start, End, time(H1, M1)),
	%if there is a simple path from start to finish it will take that instead
        departure(Start, End, H1, M1, _Throwh, _Throwm),
	!.

fly(Start, End):-
%code is really simple

	%finds a path while keeping track of visited nodes, every time you fly the been list is reset
	listpath(Start, End, Out),
	
	%writepath(Out),
	
	%Gets the time of the first flight, time of the first term to the second term
	%in the list: Out. Puts them in H1 and M1
	get_first_flight_time(Out, H1, M1),
	
	%does what the name implies, it pops the head off of the list and puts it in
	%head and puts the rest of the list in tail
	popHead(Out, Head, Tail),

	%so now we have a list of the path we need to take, the start and the starting
	%time
	
	%in here it actually traverses the path given in the list and calls the 
	%departure and arrival
	traverse(Head, H1, M1, Tail),	
	!.

main:- read(A),read(B), fly(A,B).
