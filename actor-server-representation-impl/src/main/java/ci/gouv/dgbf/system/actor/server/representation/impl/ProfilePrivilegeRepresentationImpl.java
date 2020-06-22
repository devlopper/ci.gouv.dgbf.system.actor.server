package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfilePrivilege;
import ci.gouv.dgbf.system.actor.server.representation.api.ProfilePrivilegeRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ProfilePrivilegeDto;

import org.cyk.utility.__kernel__.business.EntitySaver;
import org.cyk.utility.__kernel__.business.EntitySaver.Arguments;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.mapping.MappingHelper;
import org.cyk.utility.__kernel__.rest.ResponseBuilder;
import org.cyk.utility.__kernel__.runnable.Runner;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ProfilePrivilegeRepresentationImpl extends AbstractRepresentationEntityImpl<ProfilePrivilegeDto> implements ProfilePrivilegeRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response save(Collection<ProfilePrivilegeDto> profilePrivilegeDtos) {
		if(CollectionHelper.isEmpty(profilePrivilegeDtos))
			return Response.ok().build();
		final Collection<ProfilePrivilege> creatables = new ArrayList<>();
		final Collection<ProfilePrivilege> updatables = new ArrayList<>();
		final Collection<ProfilePrivilege> deletables = new ArrayList<>();		
		for(ProfilePrivilegeDto index : profilePrivilegeDtos) {
			ProfilePrivilege profilePrivilege = MappingHelper.getDestination(index, ProfilePrivilege.class);
			if(StringHelper.isBlank(index.getIdentifier()))
				creatables.add(profilePrivilege);
			else if(Boolean.TRUE.equals(index.get__deletable__()))
				deletables.add(profilePrivilege);
			else
				updatables.add(profilePrivilege);
		}
		Runner.Arguments runnerArguments = new Runner.Arguments().addRunnables(new Runnable() {				
			@Override
			public void run() {
				Arguments<ProfilePrivilege> arguments = new Arguments<ProfilePrivilege>();
				arguments.setPersistenceArguments(new org.cyk.utility.__kernel__.persistence.EntitySaver.Arguments<ProfilePrivilege>().setCreatables(creatables).setUpdatables(updatables).setDeletables(deletables));
				EntitySaver.getInstance().save(ProfilePrivilege.class, arguments);
			}
		});
		Runner.getInstance().run(runnerArguments);
		if(runnerArguments.getThrowable() == null)
			return Response.ok(/*ProfilePrivileges.size()+" funding source lessors has been processed"*/).build();
		return ResponseBuilder.getInstance().build(runnerArguments.getThrowable());
	}
}