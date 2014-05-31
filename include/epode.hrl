%% An epode dictionary is used to represent internal Objects.
-type epode_dict_type() :: vbisect | dict | orddict.
-type epode_bdict ()    :: vbisect:bindict().     % binary dictionary
-type epode_edict ()    :: dict().                % OTP dictionary
-type epode_odict ()    :: orddict:orddict().     % OTP orddict
-type epode_dict  ()    :: epode_bdict()
                         | epode_edict()
                         | epode_odict().

-type epode_dict_instance() :: epode_dict().

%% -type epode(Epode_Type, Key_Type, Val_Type) :: {Epode_Type, epode_dict(Key_Type, Val_Type)}.

%% Components of Objects
-type epode_db_module()        :: module().
-type epode_db_attr()          :: binary().
-type epode_db_value()         :: binary().
-type epode_db_display_attr()  :: binary().
-type epode_db_display_value() :: epode_db_value().  %% Allows conversion of display => value.
-type epode_id()               :: epode_db_value().  %% Allows conversion of display | value => token.

%% The complete set of Object types (abstracted from application/db/other).
%% -type epode_full_type()        :: ttobj_group.
%% -type epode_core_type()        :: ttobj_group_display.
