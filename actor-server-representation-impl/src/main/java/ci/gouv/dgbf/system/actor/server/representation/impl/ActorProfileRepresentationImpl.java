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
import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfile;
import ci.gouv.dgbf.system.actor.server.representation.api.ActorProfileRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ActorProfileDto;

@ApplicationScoped
public class ActorProfileRepresentationImpl extends AbstractRepresentationEntityImpl<ActorProfileDto> implements ActorProfileRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response save(Collection<ActorProfileDto> actorProfileDtos) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						if(CollectionHelper.isEmpty(actorProfileDtos))
							throw new RuntimeException("(acteur,profile) obligatoire");
						final Collection<ActorProfile> creatables = new ArrayList<>();
						final Collection<ActorProfile> updatables = new ArrayList<>();
						final Collection<ActorProfile> deletables = new ArrayList<>();		
						for(ActorProfileDto index : actorProfileDtos) {
							ActorProfile actorProfile = MappingHelper.getDestination(index, ActorProfile.class);
							if(StringHelper.isBlank(index.getIdentifier()))
								creatables.add(actorProfile);
							else if(Boolean.TRUE.equals(index.get__deletable__()))
								deletables.add(actorProfile);
							else
								updatables.add(actorProfile);
						}
						Arguments<ActorProfile> arguments = new Arguments<ActorProfile>();
						arguments.setPersistenceArguments(new org.cyk.utility.__kernel__.persistence.EntitySaver.Arguments<ActorProfile>().setCreatables(creatables)
								.setUpdatables(updatables).setDeletables(deletables));
						EntitySaver.getInstance().save(ActorProfile.class, arguments);
					}
				};
			}
		});
	}
}