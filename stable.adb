procedure STABLE is

  init_0: Boolean := true;

  -- INPUTS (PRE)

  -- VARS
  count: Integer;

  -- VARS (PRE)
  pre_count: Integer;

  -- OUTPUTS (PRE)
  pre_level: Boolean;

procedure step
 (set: Boolean;
  delay0: Integer;
  level: out Boolean) is
begin
  count := (if set then delay0
            else (if (if init_0 then false else pre_level)
                  then ((pre_count) - 1)
                  else 0));
  level := (count > 0);
  pre_count := count;
  pre_level := level;
  init_0 := false;
end;

begin
  null;
end STABLE;