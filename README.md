# cowtracks

[WIP] Tracks cowboy requests. 

Information about requests is written to an ets table. This table is
regularly scanned and information on complete requess are passed to
a pluggable handler and deleted from the ets table. 
