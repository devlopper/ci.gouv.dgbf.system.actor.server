package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.mapping.MappingHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.__kernel__.rest.ResponseBuilder;
import org.cyk.utility.__kernel__.runnable.Runner;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.ActorBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ActorPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.FunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.ProfilePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.representation.api.ActorRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ActorDto;

@ApplicationScoped
public class ActorRepresentationImpl extends AbstractRepresentationEntityImpl<ActorDto> implements ActorRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response createPrivilegesFromFunctions(Collection<ActorDto> actors) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						if(CollectionHelper.isEmpty(actors))
							throw new RuntimeException("Acteurs obligatoire");
						Collection<Actor> persistences = null;
						Collection<Function> functions = null;
						for(ActorDto dto : actors) {
							Actor actor = __inject__(ActorPersistence.class).readByBusinessIdentifier(dto.getCode());
							if(actor == null)
								throw new RuntimeException("L'utilisateur <<"+dto.getCode()+">> n'existe pas");
							if(persistences == null)
								persistences = new ArrayList<>();
							if(functions == null) {
								functions = new ArrayList<>();
								for(String code : dto.getFunctionsCodes()) {
									functions.add(__inject__(FunctionPersistence.class).readByBusinessIdentifier(code));
								}
							}
							persistences.add(actor);
						}
						__inject__(ActorBusiness.class).createPrivilegesFromFunctions(persistences, functions);
					}
				};
			}
		});
	}
	
	@Override
	public Response createProfiles(Collection<ActorDto> actors) {
		return createOrDeleteProfiles(actors, Boolean.TRUE);
	}
	
	@Override
	public Response deleteProfiles(Collection<ActorDto> actors) {
		return createOrDeleteProfiles(actors, Boolean.FALSE);
	}
	
	private static Response createOrDeleteProfiles(Collection<ActorDto> actors,Boolean create) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						if(CollectionHelper.isEmpty(actors))
							throw new RuntimeException("Acteurs obligatoire");
						Collection<Actor> persistences = null;
						Collection<Profile> profiles = null;
						for(ActorDto dto : actors) {
							Actor actor = __inject__(ActorPersistence.class).readByBusinessIdentifier(dto.getCode());
							if(actor == null)
								throw new RuntimeException("L'utilisateur <<"+dto.getCode()+">> n'existe pas");
							if(persistences == null)
								persistences = new ArrayList<>();
							if(profiles == null) {
								profiles = new ArrayList<>();
								for(String code : dto.getProfilesCodes()) {
									Profile profile = __inject__(ProfilePersistence.class).readByBusinessIdentifier(code);
									if(profile != null)
										profiles.add(profile);
								}
							}
							persistences.add(actor);
						}
						if(Boolean.TRUE.equals(create))
							__inject__(ActorBusiness.class).createProfiles(persistences, profiles);
						else
							__inject__(ActorBusiness.class).deleteProfiles(persistences, profiles);
					}
				};
			}
		});
	}
	
	@Override
	public Response getProfileInformationsByCode(String code) {
		if(StringHelper.isBlank(code))
			return Response.status(Status.BAD_REQUEST).entity("nom d'utilisateur obligatoire").build();
		Actor actor = ActorQuerier.getInstance().readProfileInformationsByCode(code);
		if(actor == null)
			return Response.status(Status.BAD_REQUEST).entity("nom d'utilisateur <<"+code+">> inconnu").build();
		if(StringHelper.isBlank(actor.getNames()))
			actor.setNames(Identity.getNames(null,actor.getFirstName(),actor.getLastNames()));
		ActorDto actorDto = MappingHelper.getSource(actor, ActorDto.class);
		return ResponseBuilder.getInstance().build(new ResponseBuilder.Arguments().setEntity(actorDto));
	}

	@Override
	public Response importFromKeycloak() {
		Integer[] count = {null};
		Runner.Arguments runnerArguments = new Runner.Arguments().addRunnables(new Runnable() {				
			@Override
			public void run() {
				count[0] = __inject__(ActorBusiness.class).importFromKeycloak();
			}
		});
		Runner.getInstance().run(runnerArguments);
		if(runnerArguments.getThrowable() == null)
			return Response.ok(NumberHelper.getInteger(count[0], 0)+" user(s) imported").build();
		return ResponseBuilder.getInstance().build(runnerArguments.getThrowable());
	}
	
	@Override
	public Response exportToKeycloak() {
		Integer[] count = {null};
		Runner.Arguments runnerArguments = new Runner.Arguments().addRunnables(new Runnable() {				
			@Override
			public void run() {
				count[0] = __inject__(ActorBusiness.class).exportToKeycloak();
			}
		});
		Runner.getInstance().run(runnerArguments);
		if(runnerArguments.getThrowable() == null)
			return Response.ok(NumberHelper.getInteger(count[0], 0)+" actor(s) exported").build();
		return ResponseBuilder.getInstance().build(runnerArguments.getThrowable());
	}
	
	@Override
	public Response updateToKeycloak() {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {						
						__inject__(ActorBusiness.class).updateToKeycloak();
					}
				};
			}
		});
	}
	
	@Override
	public Response sendUpdatePasswordEmail(String code) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						if(StringHelper.isBlank(code))
							throw new RuntimeException("Le code est obligatoire");
						Actor actor = __inject__(ActorPersistence.class).readByBusinessIdentifier(code);
						if(actor == null)
							throw new RuntimeException("L'utilisateur <<"+code+">> n'existe pas");
						__inject__(ActorBusiness.class).sendUpdatePasswordEmail(actor);
					}
				};
			}
		});
	}
}