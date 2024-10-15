# nes_lter_zooplankton_inventory_v2
Scripts to download data, merge, and tidy data to create zooplankton inventory data of zooplankton samples collected with ring and/or Bongo nets. NES-LTER. 

Scripts should be run in this order for best results:
1. Ship_speed_data.R
	a. This one can be done before or after ship speed, doesn’t matter
2. All_zp_net_bongo_logs.R
	a. This one can be done before or after ship speed, doesn’t matter
3. Shipspeed_eventlog_merge.R
4. bongoEventLogMerge.R
5. Volume_water_sampled_calculations.R
