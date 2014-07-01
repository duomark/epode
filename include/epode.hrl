%% Components of DB Objects
-type epode_db_module()      :: module().
-type epode_db_attr()        :: binary().
-type epode_db_value()       :: binary().
-type epode_db_core_attr()   :: binary().
-type epode_db_core_value()  :: epode_db_value().   %% Allows conversion of core => value.

%% Components of App Objects
-type epode_app_module()     :: module().
-type epode_app_attr()       :: atom().
-type epode_app_value()      :: any().
-type epode_app_core_attr()  :: atom().
-type epode_app_core_value() :: epode_app_value().  %% Allows conversion of core => value.

%% Generic DB/App object interfaces
-type epode_id()    :: epode_db_value().  %% Allows conversion of core | value => token.
-type epode_attr()  :: epode_db_attr()  | epode_db_core_attr()  | epode_app_attr()  | epode_app_core_attr().
-type epode_value() :: epode_db_value() | epode_db_core_value() | epode_app_value() | epode_app_core_value().

%% An epode dictionary is used to represent internal Objects.
-type epode_keyval_binary_type()     :: pure_binary.  % binary attributes and values only
-type epode_keyval_atom_type()       :: atom_attrs.   % atoms for attributes with any values
-type epode_keyval_any_type()        :: any.          % any type for both attributes and values

-type epode_keyval_non_binary_type() :: epode_keyval_atom_type()   | epode_keyval_any_type().
-type epode_keyval_type()            :: epode_keyval_binary_type() | epode_keyval_non_binary_type().

-type epode_any_dict_type()  :: dict | orddict.
-type epode_atom_dict_type() :: dict | orddict.
-type epode_edict_type()     :: dict | orddict.
-type epode_bdict_type()     :: dict | orddict | vbisect.
-type epode_all_dict_type()  :: dict | orddict | vbisect.

-type epode_bare_edict()     :: dict() | orddict:orddict().                      % erlang dict
-type epode_bare_bdict()     :: dict() | orddict:orddict() | vbisect:bindict().  % binary dict

-type epode_bdict()          :: {epode_bdict_type(), epode_keyval_binary_type(),     epode_bare_bdict()}.
-type epode_edict()          :: {epode_edict_type(), epode_keyval_non_binary_type(), epode_bare_edict()}.

-type epode_bare_dict()      :: epode_bare_edict() | epode_bare_bdict().

-type epode_any_dict()       :: epode_edict().
-type epode_atom_dict()      :: epode_edict().
-type epode_bin_dict()       :: epode_bdict().

-type epode_dict()           :: epode_any_dict() | epode_atom_dict() | epode_bin_dict().

%% An epode "object" is an epode_dict() tagged with a user defined "object type"
-type user_defined_epode_type() :: atom().
-type epode(User_Defined_Epode_Type, Epode_Dict) :: {User_Defined_Epode_Type, Epode_Dict}.
