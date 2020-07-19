package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.configuration.ConfigurationHelper;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.__kernel__.security.keycloak.User;
import org.cyk.utility.__kernel__.security.keycloak.UserManager;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.value.ValueHelper;
import org.cyk.utility.__kernel__.variable.VariableName;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessFunctionCreator;
import org.cyk.utility.server.business.BusinessFunctionRemover;

import ci.gouv.dgbf.system.actor.server.business.api.ActorBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ActorProfileBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ActorScopeBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.IdentityBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ProfileBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ActorPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.ProfileTypePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorProfileQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.PrivilegeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity.Interface;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;

@ApplicationScoped
public class ActorBusinessImpl extends AbstractBusinessEntityImpl<Actor, ActorPersistence> implements ActorBusiness,Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public Integer importFromKeycloak() {
		Collection<User> users = UserManager.getInstance().readAll();
		if(CollectionHelper.isEmpty(users))
			return null;
		Collection<Actor> actors = null;
		for(User user : users) {
			if(StringHelper.isBlank(user.getName()) || StringHelper.isBlank(user.getElectronicMailAddress()))
				continue;
			Actor actor = __inject__(ActorPersistence.class).readByBusinessIdentifier(user.getName());
			if(actor != null)
				continue;
			if(actors == null)
				actors = new ArrayList<>();
			actors.add(new Actor()
					.setKeycloakUserCreatable(Boolean.FALSE)
					.setCode(user.getName())
					.setFirstName(ValueHelper.defaultToIfBlank(user.getFirstName(), user.getName()))
					.setLastNames(ValueHelper.defaultToIfBlank(user.getLastNames(), user.getName()))
					.setElectronicMailAddress(user.getElectronicMailAddress())
				);
		}
		LogHelper.logInfo("Number of users to import from keycloak is "+CollectionHelper.getSize(actors),getClass());
		if(CollectionHelper.isNotEmpty(actors))
			createMany(actors);
		return CollectionHelper.getSize(actors);
	}
	
	@Override
	public Integer exportToKeycloak() {
		Collection<Actor> actors = __inject__(ActorPersistence.class).read();
		if(CollectionHelper.isEmpty(actors))
			return null;
		Collection<String> existingUsersNames = UserManager.getInstance().readAllNames();
		Collection<User> users = null;
		for(Actor actor : actors) {
			if(CollectionHelper.contains(existingUsersNames, actor.getCode()))
				continue;
			if(users == null)
				users = new ArrayList<>();
			users.add(new User().setName(actor.getCode()).setFirstName(actor.getFirstName()).setLastNames(actor.getLastNames())
					.setElectronicMailAddress(actor.getElectronicMailAddress()).setPass(ActorBusinessImpl.DEFAULT_PASSWORD));
		}		
		LogHelper.logInfo("Number of actors to export to keycloak is "+CollectionHelper.getSize(users),getClass());
		if(CollectionHelper.isNotEmpty(users))
			UserManager.getInstance().create(users);
		return CollectionHelper.getSize(users);
	}
	
	@Override
	protected void __listenExecuteCreateBefore__(Actor actor, Properties properties,BusinessFunctionCreator function) {
		super.__listenExecuteCreateBefore__(actor, properties, function);
		//we create identity first
		if(actor.getIdentity() == null)
			actor.setIdentity(__inject__(IdentityBusiness.class).createFromInterface((Interface) actor));
		if(StringHelper.isBlank(actor.getCode()))
			actor.setCode(actor.getIdentity().getElectronicMailAddress());
		if(StringHelper.isBlank(actor.getIdentifier()))
			actor.setIdentifier("ACT_"+actor.getCode());
	}
	
	@Override
	protected void __listenExecuteCreateAfter__(Actor actor, Properties properties, BusinessFunctionCreator function) {
		super.__listenExecuteCreateAfter__(actor, properties, function);
		//we instantiate user profile
		Profile profile = new Profile().setType(__inject__(ProfileTypePersistence.class).readByBusinessIdentifier(ProfileType.CODE_UTILISATEUR));
		profile.setCode(actor.getCode());
		profile.setName("Profile de "+actor.getIdentity().getElectronicMailAddress());
		//we collect predefined privileges from system profiles based on given functions
		if(CollectionHelper.isNotEmpty(actor.getFunctions())) {
			Collection<Privilege> privileges = PrivilegeQuerier.getInstance().readByProfilesTypesCodesByFunctionsCodes(List.of(ProfileType.CODE_SYSTEME)
					, actor.getFunctions().stream().map(x -> x.getCode()).collect(Collectors.toSet()));			
			if(CollectionHelper.isNotEmpty(privileges))								
				profile.getPrivileges(Boolean.TRUE).addAll(privileges);
		}
		__inject__(ProfileBusiness.class).create(profile);
		__inject__(ActorProfileBusiness.class).create(new ActorProfile().setActor(actor).setProfile(profile));
		
		// Integration to keycloak
		if((actor.getKeycloakUserCreatable() == null || Boolean.TRUE.equals(actor.getKeycloakUserCreatable())) && Boolean.TRUE.equals(ConfigurationHelper.is(VariableName.KEYCLOAK_ENABLED)))
			try {
				createKeycloakUser(actor);
			} catch (Exception exception) {
				LogHelper.log(exception, getClass());
			}
	}
	
	private void createKeycloakUser(Actor actor) {
		//is actor has an keycloak account ?
		User user = UserManager.getInstance().readByUserName(actor.getCode());
		if(user != null) {
			LogHelper.logInfo(String.format("user with name <<%s>> already exists in keycloak", actor.getCode()), getClass());
			return;
		}
		//we create keyclock user
		user = new User();
		user.setElectronicMailAddress(actor.getElectronicMailAddress());
		user.setFirstName(actor.getFirstName());
		user.setLastNames(actor.getLastNames());
		user.setName(actor.getCode());
		user.setPass(ValueHelper.defaultToIfBlank(actor.getPassword(),DEFAULT_PASSWORD));
		UserManager.getInstance().create(user);
	}
	
	@Override
	protected void __listenExecuteDeleteBefore__(Actor actor, Properties properties,BusinessFunctionRemover function) {
		super.__listenExecuteDeleteBefore__(actor, properties, function);
		Collection<String> actorsCodes = List.of(actor.getCode());
		Collection<ActorScope> actorScopes = ActorScopeQuerier.getInstance().readByActorsCodes(actorsCodes);
		if(CollectionHelper.isNotEmpty(actorScopes))
			__inject__(ActorScopeBusiness.class).deleteMany(actorScopes);
		Collection<ActorProfile> actorProfiles = ActorProfileQuerier.getInstance().readByActorsCodes(actorsCodes);
		if(CollectionHelper.isNotEmpty(actorProfiles)) {
			Collection<Profile> profiles = actorProfiles.stream().map(ActorProfile::getProfile).collect(Collectors.toSet());
			__inject__(ActorProfileBusiness.class).deleteMany(actorProfiles);
			__inject__(ProfileBusiness.class).deleteMany(profiles);
		}
	}
	
	@Override
	protected void __listenExecuteDeleteAfter__(Actor actor, Properties properties, BusinessFunctionRemover function) {
		super.__listenExecuteDeleteAfter__(actor, properties, function);
		__inject__(IdentityBusiness.class).delete(actor.getIdentity());
	}
	
	@Override
	protected Boolean __isCallDeleteByInstanceOnDeleteByIdentifier__() {
		return Boolean.TRUE;
	}
	
	/**/
	
	private static final String DEFAULT_PASSWORD = "123";
}