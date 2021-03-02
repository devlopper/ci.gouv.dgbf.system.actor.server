package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.business.server.EntitySaver;
import org.cyk.utility.business.server.EntitySaver.Arguments;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.mapping.MappingHelper;
import org.cyk.utility.__kernel__.rest.ResponseBuilder;
import org.cyk.utility.__kernel__.runnable.Runner;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileFunction;
import ci.gouv.dgbf.system.actor.server.representation.api.ProfileFunctionRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ProfileFunctionDto;

@ApplicationScoped
public class ProfileFunctionRepresentationImpl extends AbstractRepresentationEntityImpl<ProfileFunctionDto> implements ProfileFunctionRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public Response save(Collection<ProfileFunctionDto> profileFunctionDtos) {
		if(CollectionHelper.isEmpty(profileFunctionDtos))
			return Response.ok().build();
		final Collection<ProfileFunction> creatables = new ArrayList<>();
		final Collection<ProfileFunction> updatables = new ArrayList<>();
		final Collection<ProfileFunction> deletables = new ArrayList<>();		
		for(ProfileFunctionDto index : profileFunctionDtos) {
			ProfileFunction profileFunction = MappingHelper.getDestination(index, ProfileFunction.class);
			if(StringHelper.isBlank(index.getIdentifier()))
				creatables.add(profileFunction);
			else if(Boolean.TRUE.equals(index.get__deletable__()))
				deletables.add(profileFunction);
			else
				updatables.add(profileFunction);
		}
		Runner.Arguments runnerArguments = new Runner.Arguments().addRunnables(new Runnable() {				
			@Override
			public void run() {
				Arguments<ProfileFunction> arguments = new Arguments<ProfileFunction>();
				arguments.setPersistenceArguments(new org.cyk.utility.persistence.query.EntitySaver.Arguments<ProfileFunction>().setCreatables(creatables).setUpdatables(updatables).setDeletables(deletables));
				EntitySaver.getInstance().save(ProfileFunction.class, arguments);
			}
		});
		Runner.getInstance().run(runnerArguments);
		if(runnerArguments.getThrowable() == null)
			return Response.ok(/*ProfileFunctions.size()+" funding source lessors has been processed"*/).build();
		return ResponseBuilder.getInstance().build(runnerArguments.getThrowable());
	}
	
}
