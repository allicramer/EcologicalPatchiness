using Unitful
using DataFrames, DataFramesMeta
using CSV
using Plots, StatsPlots


module OrganismUnits
    using Unitful
    # Reference unit for single entity
    @refunit entity "entity" Entity Unitful.𝐍 true

    # Units for specific organism types.  All are equal to one entity, but
    # the different types enforce apples/orages correctness.
    organisms = ["Acorn", "Bacteria", "Beaver", "Beetle", "Beetle", "Bird",
        "Caddisfly", "Caterpillar", "Cell", "Cockle", "Copepod", "Cladoceran", "Crab",
        "Ctenophore", "Elk", "Fish", "Fly", "Gazelle", "Insect", "Locust",
        "Moose", "Mouse", "Mussel", "Nut", "Plant", "Seal", "Seastar", "Seed",
        "Shark", "Squirrel", "Superpatch", "Tree", "Tuna", "Turd", "Whale",
        "Wolf", "Worm"]
    for org in organisms
        abbr = lowercase(org)
        symb = Symbol(abbr)
        name = Symbol(org)
        @eval @unit $symb $abbr $name 1entity true
    end

    # a couple of (approximate) time units that aren't defined in base Unitful
    @unit yr "yr" Year 365.0u"d" false
    @unit mo "mo" Month 30.0u"d" false

    function __init__()
        Unitful.register(OrganismUnits)
    end
end
# using Main.OrganismUnits
string(unit(3.2 * u"elk/yr"))


# helper function to parse units straight from a string
function parseunits(u::AbstractString)
    @eval @u_str($u)
end
parseunits(u::Missing) = missing


raw = CSV.read("data/PatchinessData_QC.csv", missingstring="NA")
meta_vars = [:timestamp, :contributor, :consumer_resource_pair, :genus,
    :species, :ecosystem, :interaction_system, :consumer_type]
consolidated = raw[!, meta_vars]

measurements = [:speed, :turning_interval, :generation_time, :consumption_rate,
    :mortality_rate, :consumer_body_size, :consumer_body_mass,
    :patch_duration, :patch_length_scale, :resource_body_size, :resource_body_mass,
    :resource_density, :consumer_density, :tsearch, :tconsumption]

for meas in measurements
    meas_units = Symbol(meas, "_units")
    consolidated[!, meas] = raw[!, meas] .* parseunits.(raw[!, meas_units])
end
consolidated[!, :patch_duration] = uconvert.(u"s", consolidated[!, :patch_duration])

consolidated = @transform(consolidated,
    patch_length_scale = uconvert.(u"m", :patch_length_scale),
    patch_duration = uconvert.(u"s", :patch_duration),
    tsearch_diff = uconvert.(u"s", :patch_length_scale.^2 ./ (:speed.^2 .* :turning_interval)),
    tsearch_dir = uconvert.(u"s", coalesce.(:tsearch, :patch_length_scale ./ :speed)),
    treprod = uconvert.(u"s", :generation_time),
    tconsumption = uconvert.(u"s",
        coalesce.(:tconsumption, :resource_density ./ (:consumer_density .* :consumption_rate))),
    consumer_body_mass = uconvert.(u"g", :consumer_body_mass),
    cosumer_body_size = uconvert.(u"m", :consumer_body_size),
    resource_body_mass = uconvert.(u"g", :resource_body_mass),
    resource_body_size = uconvert.(u"m", :resource_body_size))

consolidated = @transform(consolidated,
    Fr_diff = :patch_duration ./ :tsearch_diff,
    Fr_dir = :patch_duration ./ :tsearch_dir,
    Str = :patch_duration ./ :treprod,
    Le = :patch_duration ./ :tconsumption)

output = consolidated[!, meta_vars]
for meas in [measurements; [:tsearch_diff, :tsearch_dir, :treprod]]
    meas_units = Symbol(meas, "_units")
    output[!, meas] = ustrip.(consolidated[!, meas])
    output[!, meas_units] = unit.(consolidated[meas])
end
output = [output consolidated[[:Fr_diff, :Fr_dir, :Str, :Le]]]
CSV.write("data/PatchinessData_processed.csv", output, missingstring="NA")
