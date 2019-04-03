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
string(unit(3.2u"elk/yr"))


# helper function to
function parseunits(u::AbstractString)
    @eval @u_str($u)
end
parseunits(u::Missing) = missing


raw = CSV.read("PatchinessData_QC.csv", missingstring="NA")
meta_vars = [:timestamp, :contributor, :consumer_resource_pair, :genus,
    :species, :ecosystem, :interaction_system, :consumer_type]
consolidated = raw[meta_vars]

measurements = [:speed, :turning_interval, :generation_time, :consumption_rate,
    :mortality_rate, :consumer_body_size, :patch_duration, :patch_length_scale,
    :resource_density, :consumer_density, :tsearch, :tconsumption]

for meas in measurements
    meas_units = Symbol(meas, "_units")
    consolidated[meas] = raw[meas] .* parseunits.(raw[meas_units])
end
consolidated[:patch_duration] = uconvert.(u"s", consolidated[:patch_duration])

consolidated = @transform(consolidated,
    patch_length_scale = uconvert.(u"m", :patch_length_scale),
    patch_duration = uconvert.(u"s", :patch_duration),
    tsearch_diff = uconvert.(u"s", :patch_length_scale.^2 ./ (:speed.^2 .* :turning_interval)),
    tsearch_dir = uconvert.(u"s", :patch_length_scale ./ :speed),
    treprod = uconvert.(u"s", :generation_time),
    tconsumption = uconvert.(u"s",:resource_density ./ (:consumer_density .* :consumption_rate)))
consolidated = @transform(consolidated,
    Fr_diff = :patch_duration ./ :tsearch_diff,
    Fr_dir = :patch_duration ./ :tsearch_dir,
    Str = :patch_duration ./ :treprod,
    Le = :patch_duration ./ :tconsumption)

output = consolidated[meta_vars]
for meas in [measurements; [:tsearch_diff, :tsearch_dir, :treprod]]
    meas_units = Symbol(meas, "_units")
    output[meas] = ustrip.(consolidated[meas])
    output[meas_units] = string.(unit.(consolidated[meas]))
end
output = [output consolidated[[:Fr_diff, :Fr_dir, :Str, :Le]]]
CSV.write("PatchinessData_processed.csv", output)


#ustrip, dimension
histogram(log10.(ustrip.(uconvert.(u"s", consolidated.patch_duration))), bins=10)

@df consolidated scatter(:Fr_dir, :Str, xscale=:log10, yscale=:log10,
    xlabel="Fr", ylabel="Str", label="Directed")
@df consolidated scatter!(:Fr_diff, :Str, xscale=:log10, yscale=:log10,
    label="Diffusive")
vline!([1.], color=:black, label="")
hline!([1.], color=:black, label="")

@df consolidated scatter(:Fr_dir, :Le, xscale=:log10, yscale=:log10,
    xlabel="Fr", ylabel="Le", label="Directed")
@df consolidated scatter!(:Fr_diff, :Le, xscale=:log10, yscale=:log10,
    label="Diffusive")
vline!([1.], color=:black, label="")
hline!([1.], color=:black, label="")

@df consolidated scatter(:Le, :Str, xscale=:log10, yscale=:log10,
    xlabel="Le", ylabel="Str", legend=false)
vline!([1.], color=:black, label="")
hline!([1.], color=:black, label="")


# 3D plot
@df consolidated scatter3d(log10.(:Fr_dir), log10.(:Str), log10.(:Le),
    xlabel="Fr", ylabel="Str", zlabel="Le", label="Directed")
@df consolidated scatter3d!(log10.(:Fr_diff), log10.(:Str), log10.(:Le),
    label="Diffusive")


@df consolidated scatter(ustrip.(:patch_length_scale), ustrip.(:patch_duration),
    xscale=:log10, yscale=:log10)
