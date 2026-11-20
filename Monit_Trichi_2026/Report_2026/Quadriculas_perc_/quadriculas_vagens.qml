<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis version="3.34.0-Prizren" styleCategories="Symbology">
  <renderer-v2 forceraster="0" type="RuleRenderer" enableorderby="0" referencescale="-1" symbollevels="0">
    <rules key="{63dbe803-beec-4fb9-95ec-3c6f97dcc6c3}">
      <rule filter="&quot;visitada&quot; = 'Sim' AND &quot;m_vagens&quot; = 0" key="{b892dea2-7390-4823-937f-39bcfba5fbba}" label="0%" symbol="0"/>
      <rule filter="&quot;m_vagens&quot; &gt; 0 AND &quot;m_vagens&quot; &lt;= 25" key="{43aa3256-e099-4c26-8f9c-5ea582e8e870}" label="1 - 25%" symbol="1"/>
      <rule filter="&quot;m_vagens&quot; &gt; 25 AND &quot;m_vagens&quot; &lt;= 75" key="{31c90667-aa00-413f-a7d4-1232d920211e}" label="25 - 75%" symbol="2"/>
      <rule filter="&quot;m_vagens&quot; &gt; 75" key="{8fd2415e-5ae4-48e3-9ce8-a5f706347f2b}" label="75 - 100%" symbol="3"/>
      <rule filter="&quot;visitada&quot; = 'Nao' AND &quot;reg_previo&quot; = 'Nao'" key="{d0b06443-e992-4557-af01-171004b229ad}" label="Grid not visited, with no previous record of the BCA" symbol="4"/>
      <rule filter="&quot;visitada&quot; = 'Nao' AND &quot;reg_previo&quot; = 'Sim'" key="{e30252f7-670f-4b55-9ce4-b8a412c7790d}" label="Grid not visited, with previous record of the BCA" symbol="5"/>
      <rule key="{590b61cb-d295-409c-aad7-f51ceb9e91be}" label="Grid UTM 10 x 10 km" symbol="6"/>
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
    <symbol type="fill" name="2" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
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
    <symbol type="fill" name="3" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
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
    <symbol type="fill" name="4" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
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
    <symbol type="fill" name="5" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
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
    <symbol type="fill" name="6" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
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