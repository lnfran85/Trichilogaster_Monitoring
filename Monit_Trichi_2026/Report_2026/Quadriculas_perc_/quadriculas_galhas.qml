<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis version="3.34.0-Prizren" styleCategories="Symbology">
  <renderer-v2 forceraster="0" type="RuleRenderer" enableorderby="0" referencescale="-1" symbollevels="0">
    <rules key="{3a4e3d0c-9baf-4219-ba5e-debc3e7bb38a}">
      <rule filter="&quot;m_galhas&quot; &gt; 0 AND &quot;m_galhas&quot; &lt;= 25" key="{765dcb02-ca8d-4b21-a479-c341d5ed6e96}" label="1 - 25%" symbol="0"/>
      <rule filter="&quot;m_galhas&quot; &gt; 25 AND &quot;m_galhas&quot; &lt;= 75" key="{4f5ca152-0756-4914-99e1-9d28519373e0}" label="25 - 75%" symbol="1"/>
      <rule filter="&quot;m_galhas&quot; &gt; 75" key="{c64670b6-73c2-4563-8d22-002e3490aa47}" label="75 - 100%" symbol="2"/>
      <rule filter="&quot;visitada&quot; = 'Nao' AND &quot;reg_previo&quot; = 'Nao'" key="{6387ba86-e104-4648-b4c2-efcae60c1d72}" label="Grid not visited, with no previous record of the BCA" symbol="3"/>
      <rule filter="&quot;visitada&quot; = 'Nao' AND &quot;reg_previo&quot; = 'Sim'" key="{4a528ed9-2c94-430b-b417-552d79dfc0d5}" label="Grid not visited, with previous record of the BCA" symbol="4"/>
      <rule key="{4761a0ae-254d-4d98-b3dc-b20eed0a2e82}" label="Grid UTM 10 x 10 km" symbol="5"/>
    </rules>
    <symbols>
    <symbol type="fill" name="0" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
      <data_defined_properties><Option type="Map"><Option type="QString" name="name" value=""/><Option name="properties"/><Option type="QString" name="type" value="collection"/></Option></data_defined_properties>
      <layer class="SimpleFill" locked="0" pass="0" enabled="1"><Option type="Map">
        <Option type="QString" name="border_width_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="color" value="255,255,178,255"/>
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
    <symbol type="fill" name="2" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
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
    <symbol type="fill" name="3" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
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
    <symbol type="fill" name="4" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
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
    <symbol type="fill" name="5" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
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