
-record(point,
	{id :: string(),
	 time :: integer(),
	 latitude :: float(),
	 longitude :: float()
	}).

-record(path,
	{id :: string(),
	 points :: [#point{}]
	}).
