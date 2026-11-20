<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis version="3.34.0-Prizren" styleCategories="Symbology">
  <renderer-v2 forceraster="0" type="RuleRenderer" enableorderby="0" referencescale="-1" symbollevels="0">
    <rules key="{f3e3478e-fc7b-484d-82d7-7eb20f06e449}">
      <rule filter="&quot;visitada&quot; = 'Sim' AND &quot;m_galhas&quot; = 0" key="{07f89de4-1115-49d4-adc8-31636f590da5}" label="0%" symbol="0"/>
      <rule filter="&quot;m_galhas&quot; &gt; 0 AND &quot;m_galhas&quot; &lt;= 10" key="{46d9b1a6-be8b-444b-855f-9db6ad2eada9}" label="1-10%" symbol="1"/>
      <rule filter="&quot;m_galhas&quot; &gt; 10 AND &quot;m_galhas&quot; &lt;= 25" key="{0b630a48-4f0f-407a-b975-322b64d4ce16}" label="10-25%" symbol="2"/>
      <rule filter="&quot;m_galhas&quot; &gt; 25 AND &quot;m_galhas&quot; &lt;= 50" key="{e5f66c48-3f04-48bb-9c36-348978ebf597}" label="25%-50%" symbol="3"/>
      <rule filter="&quot;m_galhas&quot; &gt; 50 AND &quot;m_galhas&quot; &lt;= 75" key="{7fad09ba-2836-45c2-8bc3-b417d01eebcd}" label="50%-75%" symbol="4"/>
      <rule filter="&quot;m_galhas&quot; &gt; 75 AND &quot;m_galhas&quot; &lt;= 90" key="{4462d698-eb12-4abb-a083-c6af251dafdb}" label="75%-90%" symbol="5"/>
      <rule filter="&quot;m_galhas&quot; &gt; 90" key="{ecfcad0d-b8e1-44bd-9f81-ee8fa213df06}" label="&gt;90%" symbol="6"/>
      <rule filter="&quot;visitada&quot; = 'Nao' AND &quot;reg_previo&quot; = 'Nao'" key="{9a40e1e8-a00b-4e58-aea6-67d6b22364ce}" label="Grid não visitada, sem registo prévio do BCA" symbol="7"/>
      <rule filter="&quot;visitada&quot; = 'Nao' AND &quot;reg_previo&quot; = 'Sim'" key="{3d013032-b272-4832-abe0-541c5c33e50c}" label="Grid não visitada, com registo prévio do BCA" symbol="8"/>
      <rule key="{282f4a3e-cce9-45d2-a7fb-e6d2fce279e5}" label="Grid UTM 10 x 10 km" symbol="9"/>
    </rules>
    <symbols>
    <symbol type="fill" name="0" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
      <data_defined_properties><Option type="Map"><Option type="QString" name="name" value=""/><Option name="properties"/><Option type="QString" name="type" value="collection"/></Option></data_defined_properties>
      <layer class="SimpleFill" locked="0" pass="0" enabled="1"><Option type="Map">
        <Option type="QString" name="border_width_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="color" value="255,255,255,255"/>
        <Option type="QString" name="joinstyle" value="bevel"/>
        <Option type="QString" name="offset" value="0,0"/>
        <Option type="QString" name="offset_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="offset_unit" value="MM"/>
        <Option type="QString" name="outline_color" value="127,127,127,255"/>
        <Option type="QString" name="outline_style" value="solid"/>
        <Option type="QString" name="outline_width" value="0.15"/>
        <Option type="QString" name="outline_width_unit" value="MM"/>
        <Option type="QString" name="style" value="solid"/>
      </Option></layer>
    </symbol>
    <symbol type="fill" name="1" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
      <data_defined_properties><Option type="Map"><Option type="QString" name="name" value=""/><Option name="properties"/><Option type="QString" name="type" value="collection"/></Option></data_defined_properties>
      <layer class="SimpleFill" locked="0" pass="0" enabled="1"><Option type="Map">
        <Option type="QString" name="border_width_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="color" value="255,255,204,255"/>
        <Option type="QString" name="joinstyle" value="bevel"/>
        <Option type="QString" name="offset" value="0,0"/>
        <Option type="QString" name="offset_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="offset_unit" value="MM"/>
        <Option type="QString" name="outline_color" value="127,127,127,255"/>
        <Option type="QString" name="outline_style" value="solid"/>
        <Option type="QString" name="outline_width" value="0.15"/>
        <Option type="QString" name="outline_width_unit" value="MM"/>
        <Option type="QString" name="style" value="solid"/>
      </Option></layer>
    </symbol>
    <symbol type="fill" name="2" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
      <data_defined_properties><Option type="Map"><Option type="QString" name="name" value=""/><Option name="properties"/><Option type="QString" name="type" value="collection"/></Option></data_defined_properties>
      <layer class="SimpleFill" locked="0" pass="0" enabled="1"><Option type="Map">
        <Option type="QString" name="border_width_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="color" value="217,240,163,255"/>
        <Option type="QString" name="joinstyle" value="bevel"/>
        <Option type="QString" name="offset" value="0,0"/>
        <Option type="QString" name="offset_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="offset_unit" value="MM"/>
        <Option type="QString" name="outline_color" value="127,127,127,255"/>
        <Option type="QString" name="outline_style" value="solid"/>
        <Option type="QString" name="outline_width" value="0.15"/>
        <Option type="QString" name="outline_width_unit" value="MM"/>
        <Option type="QString" name="style" value="solid"/>
      </Option></layer>
    </symbol>
    <symbol type="fill" name="3" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
      <data_defined_properties><Option type="Map"><Option type="QString" name="name" value=""/><Option name="properties"/><Option type="QString" name="type" value="collection"/></Option></data_defined_properties>
      <layer class="SimpleFill" locked="0" pass="0" enabled="1"><Option type="Map">
        <Option type="QString" name="border_width_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="color" value="173,221,142,255"/>
        <Option type="QString" name="joinstyle" value="bevel"/>
        <Option type="QString" name="offset" value="0,0"/>
        <Option type="QString" name="offset_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="offset_unit" value="MM"/>
        <Option type="QString" name="outline_color" value="127,127,127,255"/>
        <Option type="QString" name="outline_style" value="solid"/>
        <Option type="QString" name="outline_width" value="0.15"/>
        <Option type="QString" name="outline_width_unit" value="MM"/>
        <Option type="QString" name="style" value="solid"/>
      </Option></layer>
    </symbol>
    <symbol type="fill" name="4" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
      <data_defined_properties><Option type="Map"><Option type="QString" name="name" value=""/><Option name="properties"/><Option type="QString" name="type" value="collection"/></Option></data_defined_properties>
      <layer class="SimpleFill" locked="0" pass="0" enabled="1"><Option type="Map">
        <Option type="QString" name="border_width_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="color" value="120,198,121,255"/>
        <Option type="QString" name="joinstyle" value="bevel"/>
        <Option type="QString" name="offset" value="0,0"/>
        <Option type="QString" name="offset_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="offset_unit" value="MM"/>
        <Option type="QString" name="outline_color" value="127,127,127,255"/>
        <Option type="QString" name="outline_style" value="solid"/>
        <Option type="QString" name="outline_width" value="0.15"/>
        <Option type="QString" name="outline_width_unit" value="MM"/>
        <Option type="QString" name="style" value="solid"/>
      </Option></layer>
    </symbol>
    <symbol type="fill" name="5" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
      <data_defined_properties><Option type="Map"><Option type="QString" name="name" value=""/><Option name="properties"/><Option type="QString" name="type" value="collection"/></Option></data_defined_properties>
      <layer class="SimpleFill" locked="0" pass="0" enabled="1"><Option type="Map">
        <Option type="QString" name="border_width_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="color" value="49,163,84,255"/>
        <Option type="QString" name="joinstyle" value="bevel"/>
        <Option type="QString" name="offset" value="0,0"/>
        <Option type="QString" name="offset_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="offset_unit" value="MM"/>
        <Option type="QString" name="outline_color" value="127,127,127,255"/>
        <Option type="QString" name="outline_style" value="solid"/>
        <Option type="QString" name="outline_width" value="0.15"/>
        <Option type="QString" name="outline_width_unit" value="MM"/>
        <Option type="QString" name="style" value="solid"/>
      </Option></layer>
    </symbol>
    <symbol type="fill" name="6" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
      <data_defined_properties><Option type="Map"><Option type="QString" name="name" value=""/><Option name="properties"/><Option type="QString" name="type" value="collection"/></Option></data_defined_properties>
      <layer class="SimpleFill" locked="0" pass="0" enabled="1"><Option type="Map">
        <Option type="QString" name="border_width_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="color" value="0,104,55,255"/>
        <Option type="QString" name="joinstyle" value="bevel"/>
        <Option type="QString" name="offset" value="0,0"/>
        <Option type="QString" name="offset_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="offset_unit" value="MM"/>
        <Option type="QString" name="outline_color" value="127,127,127,255"/>
        <Option type="QString" name="outline_style" value="solid"/>
        <Option type="QString" name="outline_width" value="0.15"/>
        <Option type="QString" name="outline_width_unit" value="MM"/>
        <Option type="QString" name="style" value="solid"/>
      </Option></layer>
    </symbol>
    <symbol type="fill" name="7" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
      <data_defined_properties><Option type="Map"><Option type="QString" name="name" value=""/><Option name="properties"/><Option type="QString" name="type" value="collection"/></Option></data_defined_properties>
      <layer class="SimpleFill" locked="0" pass="0" enabled="1"><Option type="Map">
        <Option type="QString" name="border_width_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="color" value="255,127,0,255"/>
        <Option type="QString" name="joinstyle" value="bevel"/>
        <Option type="QString" name="offset" value="0,0"/>
        <Option type="QString" name="offset_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="offset_unit" value="MM"/>
        <Option type="QString" name="outline_color" value="127,127,127,255"/>
        <Option type="QString" name="outline_style" value="solid"/>
        <Option type="QString" name="outline_width" value="0.15"/>
        <Option type="QString" name="outline_width_unit" value="MM"/>
        <Option type="QString" name="style" value="solid"/>
      </Option></layer>
    </symbol>
    <symbol type="fill" name="8" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
      <data_defined_properties><Option type="Map"><Option type="QString" name="name" value=""/><Option name="properties"/><Option type="QString" name="type" value="collection"/></Option></data_defined_properties>
      <layer class="SimpleFill" locked="0" pass="0" enabled="1"><Option type="Map">
        <Option type="QString" name="border_width_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="color" value="0,0,0,255"/>
        <Option type="QString" name="joinstyle" value="bevel"/>
        <Option type="QString" name="offset" value="0,0"/>
        <Option type="QString" name="offset_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="offset_unit" value="MM"/>
        <Option type="QString" name="outline_color" value="127,127,127,255"/>
        <Option type="QString" name="outline_style" value="solid"/>
        <Option type="QString" name="outline_width" value="0.15"/>
        <Option type="QString" name="outline_width_unit" value="MM"/>
        <Option type="QString" name="style" value="solid"/>
      </Option></layer>
    </symbol>
    <symbol type="fill" name="9" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
      <data_defined_properties><Option type="Map"><Option type="QString" name="name" value=""/><Option name="properties"/><Option type="QString" name="type" value="collection"/></Option></data_defined_properties>
      <layer class="SimpleFill" locked="0" pass="0" enabled="1"><Option type="Map">
        <Option type="QString" name="border_width_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="color" value="0,0,0,0"/>
        <Option type="QString" name="joinstyle" value="bevel"/>
        <Option type="QString" name="offset" value="0,0"/>
        <Option type="QString" name="offset_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="offset_unit" value="MM"/>
        <Option type="QString" name="outline_color" value="127,127,127,255"/>
        <Option type="QString" name="outline_style" value="solid"/>
        <Option type="QString" name="outline_width" value="0.15"/>
        <Option type="QString" name="outline_width_unit" value="MM"/>
        <Option type="QString" name="style" value="no"/>
      </Option></layer>
    </symbol>
    </symbols>
  </renderer-v2>
  <layerGeometryType>2</layerGeometryType>
</qgis>