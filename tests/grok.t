Generate ATD types from grok (Grafana Object Development Kit) dashboard types
  $ jsonschema2atd --format openapi ./mocks/dashboard_types_gen.json
  
  type json <ocaml module="Yojson.Basic" t="t"> = abstract
  type int64 = int <ocaml repr="int64">
  
  type fieldConfigSourceOverrides = {
    matcher: matcherConfig;
    properties: dynamicConfigValue list;
  }
  
  type graphPanelType = [
    | Graph <json name="graph">
  ]
  
  type graphPanelLegend = {
    ~show <ocaml default="true">: bool;
    ?sort: string option;
    ?sortDesc: bool option;
  }
  
  type heatmapPanelType = [
    | Heatmap <json name="heatmap">
  ]
  
  type panelRepeatDirection = [
    | H <json name="h">
    | V <json name="v">
  ]
  
  type rangeMapType = [
    | Range <json name="range">
  ]
  
  type rangeMapOptions = {
    from: float;
    to_ <json name="to">: float;
    result: valueMappingResult;
  }
  
  type regexMapType = [
    | Regex <json name="regex">
  ]
  
  type regexMapOptions = {
    pattern: string;
    result: valueMappingResult;
  }
  
  type rowPanelType = [
    | Row <json name="row">
  ]
  
  type rowPanelPanels = [
    | Panel of panel
    | GraphPanel of graphPanel
    | HeatmapPanel of heatmapPanel
  ] <json adapter.ocaml="Jsonschema2atd_runtime.Adapter.One_of">
  
  type specialValueMapType = [
    | Special <json name="special">
  ]
  
  type specialValueMapOptions = {
    match_ <json name="match">: specialValueMatch;
    result: valueMappingResult;
  }
  
  type valueMapType = [
    | Value <json name="value">
  ]
  
  type variableModelQuery = [
    | String of string
    | Json of json
  ] <json adapter.ocaml="Jsonschema2atd_runtime.Adapter.One_of">
  
  type variableOptionText = [
    | String of string
    | StringList of string list
  ] <json adapter.ocaml="Jsonschema2atd_runtime.Adapter.One_of">
  
  type variableOptionValue = [
    | String of string
    | StringList of string list
  ] <json adapter.ocaml="Jsonschema2atd_runtime.Adapter.One_of">
  
  type dashboardMetadata = {
    updateTimestamp: string;
    createdBy: string;
    updatedBy: string;
    extraFields: json;
    uid: string;
    creationTimestamp: string;
    ?deletionTimestamp: string option;
    finalizers: string list;
    resourceVersion: string;
    labels: json;
  }
  
  type dashboardSpecTime = {
    ~from <ocaml default="\"now-6h\"">: string;
    ~to_ <json name="to"> <ocaml default="\"now\"">: string;
  }
  
  type dashboardSpecTimepicker = {
    ~hidden <ocaml default="false">: bool;
    ~refresh_intervals <ocaml default="[\"5s\";\"10s\";\"30s\";\"1m\";\"5m\";\"15m\";\"30m\";\"1h\";\"2h\";\"1d\"]">: string list;
    ~collapse <ocaml default="false">: bool;
    ~time_options <ocaml default="[\"5m\";\"15m\";\"1h\";\"6h\";\"12h\";\"24h\";\"2d\";\"7d\";\"30d\"]">: string list;
  }
  
  type dashboardSpecRefresh = [
    | Bool of bool
    | String of string
  ] <json adapter.ocaml="Jsonschema2atd_runtime.Adapter.One_of">
  
  type dashboardSpecPanels = [
    | Panel of panel
    | RowPanel of rowPanel
    | GraphPanel of graphPanel
    | HeatmapPanel of heatmapPanel
  ] <json adapter.ocaml="Jsonschema2atd_runtime.Adapter.One_of">
  
  type dashboardSpecTemplating = {
    ?list: variableModel list option;
  }
  
  type dashboardSpec = {
    ?id: int option;
    ?uid: string option;
    ?title: string option;
    ?description: string option;
    ?revision: int64 option;
    ?gnetId: string option;
    ?tags: string list option;
    ~timezone <ocaml default="\"browser\"">: string;
    ~editable <ocaml default="true">: bool;
    ?graphTooltip: dashboardCursorSync option;
    ?time: dashboardSpecTime option;
    ?timepicker: dashboardSpecTimepicker option;
    ~fiscalYearStartMonth <ocaml default="0">: int;
    ?liveNow: bool option;
    ?weekStart: string option;
    ?refresh: dashboardSpecRefresh option;
    ~schemaVersion <ocaml default="36">: int;
    ?version: int option;
    ?panels: dashboardSpecPanels list option;
    ?templating: dashboardSpecTemplating option;
    ?annotations: annotationContainer option;
    ?links: dashboardLink list option;
    ?snapshot: snapshot option;
  }
  
  type dashboardStatus = {
    ?operatorStates: json option;
    ?additionalFields: json option;
  }
  
  type statusOperatorStateState = [
    | Success <json name="success">
    | In_progress <json name="in_progress">
    | Failed <json name="failed">
  ]
  
  type statusOperatorState = {
    lastEvaluation: string;
    state: statusOperatorStateState;
    ?descriptiveState: string option;
    ?details: json option;
  }
  
  type dashboard = {
    metadata: dashboardMetadata;
    spec: dashboardSpec;
    status: dashboardStatus;
  }
  
  type _kubeObjectMetadata = {
    uid: string;
    creationTimestamp: string;
    ?deletionTimestamp: string option;
    finalizers: string list;
    resourceVersion: string;
    labels: json;
  }
  
  type variableType = [
    | Query <json name="query">
    | Adhoc <json name="adhoc">
    | Constant <json name="constant">
    | Datasource <json name="datasource">
    | Interval <json name="interval">
    | Textbox <json name="textbox">
    | Custom <json name="custom">
    | System <json name="system">
  ]
  
  type variableSort = int
  
  type variableRefresh = int
  
  type variableOption = {
    ?selected: bool option;
    text: variableOptionText;
    value: variableOptionValue;
  }
  
  type variableModel = {
    type_ <json name="type">: variableType;
    name: string;
    ?label: string option;
    ?hide: variableHide option;
    ~skipUrlSync <ocaml default="false">: bool;
    ?description: string option;
    ?query: variableModelQuery option;
    ?datasource: dataSourceRef option;
    ?current: variableOption option;
    ~multi <ocaml default="false">: bool;
    ?options: variableOption list option;
    ?refresh: variableRefresh option;
    ?sort: variableSort option;
  }
  
  type variableHide = int
  
  type valueMappingResult = {
    ?text: string option;
    ?color: string option;
    ?icon: string option;
    ?index: int option;
  }
  
  type valueMapping = [
    | ValueMap of valueMap
    | RangeMap of rangeMap
    | RegexMap of regexMap
    | SpecialValueMap of specialValueMap
  ] <json adapter.ocaml="Jsonschema2atd_runtime.Adapter.One_of">
  
  type valueMap = {
    type_ <json name="type">: valueMapType;
    options: json;
  }
  
  type thresholdsMode = [
    | Absolute <json name="absolute">
    | Percentage <json name="percentage">
  ]
  
  type thresholdsConfig = {
    mode: thresholdsMode;
    steps: threshold list;
  }
  
  type threshold = {
    value: float;
    color: string;
  }
  
  type target = json
  
  type specialValueMatch = [
    | True_ <json name="true">
    | False_ <json name="false">
    | Null <json name="null">
    | Nan <json name="nan">
    | Nullnan <json name="null+nan">
    | Empty <json name="empty">
  ]
  
  type specialValueMap = {
    type_ <json name="type">: specialValueMapType;
    options: specialValueMapOptions;
  }
  
  type snapshot = {
    created: string;
    expires: string;
    external_ <json name="external">: bool;
    externalUrl: string;
    id: int;
    key: string;
    name: string;
    orgId: int;
    updated: string;
    ?url: string option;
    userId: int;
  }
  
  type rowPanel = {
    type_ <json name="type">: rowPanelType;
    ~collapsed <ocaml default="false">: bool;
    ?title: string option;
    ?datasource: dataSourceRef option;
    ?gridPos: gridPos option;
    id: int;
    panels: rowPanelPanels list;
    ?repeat: string option;
  }
  
  type regexMap = {
    type_ <json name="type">: regexMapType;
    options: regexMapOptions;
  }
  
  type rangeMap = {
    type_ <json name="type">: rangeMapType;
    options: rangeMapOptions;
  }
  
  type panel = {
    type_ <json name="type">: string;
    ?id: int option;
    ?pluginVersion: string option;
    ?tags: string list option;
    ?targets: target list option;
    ?title: string option;
    ?description: string option;
    ~transparent <ocaml default="false">: bool;
    ?datasource: dataSourceRef option;
    ?gridPos: gridPos option;
    ?links: dashboardLink list option;
    ?repeat: string option;
    ~repeatDirection <ocaml default="`H">: panelRepeatDirection;
    ?maxPerRow: float option;
    ?maxDataPoints: float option;
    ?transformations: dataTransformerConfig list option;
    ?interval: string option;
    ?timeFrom: string option;
    ?timeShift: string option;
    ?hideTimeOverride: bool option;
    ?libraryPanel: libraryPanelRef option;
    ?options: json option;
    ?fieldConfig: fieldConfigSource option;
  }
  
  type matcherConfig = {
    ~id <ocaml default="\"\"">: string;
    ?options: json option;
  }
  
  type mappingType = [
    | Value <json name="value">
    | Range <json name="range">
    | Regex <json name="regex">
    | Special <json name="special">
  ]
  
  type libraryPanelRef = {
    name: string;
    uid: string;
  }
  
  type heatmapPanel = {
    type_ <json name="type">: heatmapPanelType;
  }
  
  type gridPos = {
    ~h <ocaml default="9">: int;
    ~w <ocaml default="12">: int;
    ~x <ocaml default="0">: int;
    ~y <ocaml default="0">: int;
    ?static: bool option;
  }
  
  type graphPanel = {
    type_ <json name="type">: graphPanelType;
    ?legend: graphPanelLegend option;
  }
  
  type fieldConfigSource = {
    defaults: fieldConfig;
    overrides: fieldConfigSourceOverrides list;
  }
  
  type fieldConfig = {
    ?displayName: string option;
    ?displayNameFromDS: string option;
    ?description: string option;
    ?path: string option;
    ?writeable: bool option;
    ?filterable: bool option;
    ?unit: string option;
    ?decimals: float option;
    ?min: float option;
    ?max: float option;
    ?mappings: valueMapping list option;
    ?thresholds: thresholdsConfig option;
    ?color: fieldColor option;
    ?links: json list option;
    ?noValue: string option;
    ?custom: json option;
  }
  
  type fieldColorSeriesByMode = [
    | Min <json name="min">
    | Max <json name="max">
    | Last <json name="last">
  ]
  
  type fieldColorModeId = [
    | Thresholds <json name="thresholds">
    | Paletteclassic <json name="palette-classic">
    | Paletteclassicbyname <json name="palette-classic-by-name">
    | ContinuousGrYlRd <json name="continuous-GrYlRd">
    | ContinuousRdYlGr <json name="continuous-RdYlGr">
    | ContinuousBlYlRd <json name="continuous-BlYlRd">
    | ContinuousYlRd <json name="continuous-YlRd">
    | ContinuousBlPu <json name="continuous-BlPu">
    | ContinuousYlBl <json name="continuous-YlBl">
    | Continuousblues <json name="continuous-blues">
    | Continuousreds <json name="continuous-reds">
    | Continuousgreens <json name="continuous-greens">
    | Continuouspurples <json name="continuous-purples">
    | Fixed <json name="fixed">
    | Shades <json name="shades">
  ]
  
  type fieldColor = {
    mode: fieldColorModeId;
    ?fixedColor: string option;
    ?seriesBy: fieldColorSeriesByMode option;
  }
  
  type dynamicConfigValue = {
    ~id <ocaml default="\"\"">: string;
    ?value: json option;
  }
  
  type dataTransformerConfig = {
    id: string;
    ?disabled: bool option;
    ?filter: matcherConfig option;
    options: json;
  }
  
  type dataSourceRef = {
    ?type_ <json name="type">: string option;
    ?uid: string option;
  }
  
  type dashboardLinkType = [
    | Link <json name="link">
    | Dashboards <json name="dashboards">
  ]
  
  type dashboardLink = {
    title: string;
    type_ <json name="type">: dashboardLinkType;
    icon: string;
    tooltip: string;
    url: string;
    tags: string list;
    ~asDropdown <ocaml default="false">: bool;
    ~targetBlank <ocaml default="false">: bool;
    ~includeVars <ocaml default="false">: bool;
    ~keepTime <ocaml default="false">: bool;
  }
  
  type dashboardCursorSync = int
  
  type annotationTarget = {
    limit: int64;
    matchAny: bool;
    tags: string list;
    type_ <json name="type">: string;
  }
  
  type annotationQuery = {
    name: string;
    datasource: dataSourceRef;
    ~enable <ocaml default="true">: bool;
    ~hide <ocaml default="false">: bool;
    iconColor: string;
    ?filter: annotationPanelFilter option;
    ?target: annotationTarget option;
    ?type_ <json name="type">: string option;
    ~builtIn <ocaml default="0">: float;
  }
  
  type annotationPanelFilter = {
    ~exclude <ocaml default="false">: bool;
    ids: int list;
  }
  
  type annotationContainer = {
    ?list: annotationQuery list option;
  }
  


