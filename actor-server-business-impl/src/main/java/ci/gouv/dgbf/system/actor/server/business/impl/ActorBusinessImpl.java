package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.enterprise.context.ApplicationScoped;
import javax.transaction.Transactional;

import org.apache.commons.lang3.StringUtils;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.configuration.ConfigurationHelper;
import org.cyk.utility.__kernel__.instance.InstanceCopier;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.__kernel__.value.ValueHelper;
import org.cyk.utility.__kernel__.variable.VariableName;
import org.cyk.utility.mail.MailSender;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.query.EntityUpdater;
import org.cyk.utility.persistence.server.query.executor.field.GenericFieldExecutor;
import org.cyk.utility.security.keycloak.server.KeycloakHelper;
import org.cyk.utility.security.keycloak.server.User;
import org.cyk.utility.security.keycloak.server.UserManager;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessFunctionCreator;
import org.cyk.utility.server.business.BusinessFunctionRemover;

import ci.gouv.dgbf.system.actor.server.business.api.ActorBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ActorProfileBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ActorScopeBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.IdentityBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ProfileBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ProfilePrivilegeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ActorPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorProfileQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity.Interface;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.impl.FreeMarker;

@ApplicationScoped
public class ActorBusinessImpl extends AbstractBusinessEntityImpl<Actor, ActorPersistence> implements ActorBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Actor instantiateOneToBeCreatedByPublic() {
		Actor actor = ActorQuerier.getInstance().instantiateOneToBeCreatedByPublic();
		return actor;
	}
	
	@Override @Transactional
	public void createByPublic(Actor actor) {
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("actor", actor);
		actor.setElectronicMailAddress(ActorBusiness.normalizeElectronicMailAddress(actor.getElectronicMailAddress()));			
		actor.setIdentity(__inject__(IdentityBusiness.class).createFromInterface((Interface) actor));
		actor.setCode(actor.getIdentity().getElectronicMailAddress());
		actor.setIdentifier("ACT_"+actor.getCode());
		actor.setCreationDate(LocalDateTime.now());
		create(actor);
		//EntityCreator.getInstance().createMany(List.of(actor));
	}
	/*
	@Override @Transactional
	public void createRequerantOfCeliopeByPublic(Actor actor) {
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("actor", actor);
		createByPublic(actor);
		ActorProfile actorProfile = new ActorProfile();
		actorProfile.setActor(actor).setProfile(CodeExecutor.getInstance().getOne(Profile.class, Profile.CODE_REQUERANT_CELIOPE));
		__inject__(ActorProfileBusiness.class).create(actorProfile);
	}
	*/
	@Override @Transactional
	public void createPrivilegesFromFunctions(Collection<Actor> actors,Collection<Function> functions) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("actors", actors);
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("functions", functions);
		Collection<Profile> profiles = ProfileQuerier.getInstance().readByActorsCodes(actors.stream().map(x -> x.getCode()).collect(Collectors.toList()));
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("profiles of actors", profiles);
		__inject__(ProfilePrivilegeBusiness.class).createFromFunctions(profiles, functions);
	}
	
	@Override @Transactional
	public void createProfiles(Collection<Actor> actors, Collection<Profile> profiles) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("actors", actors);
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("profiles", profiles);
		Collection<ActorProfile> newActorProfiles = new ArrayList<>();
		Collection<ActorProfile> existingActorProfiles = ActorProfileQuerier.getInstance().readByActorsCodes(actors.stream().map(x->x.getCode()).collect(Collectors.toList()));
		actors.forEach(actor -> {
			Collection<Profile> existingProfiles = CollectionHelper.isEmpty(existingActorProfiles) ? null : existingActorProfiles.stream().filter(x->x.getActor().equals(actor))
					.map(x->x.getProfile()).collect(Collectors.toList());
			Collection<Profile> newProfiles = CollectionHelper.isEmpty(existingProfiles) ? profiles : profiles.stream().filter(x->!existingProfiles.contains(x))
					.collect(Collectors.toList());
			if(CollectionHelper.isNotEmpty(newProfiles))
				newActorProfiles.addAll(newProfiles.stream().map(profile -> new ActorProfile().setActor(actor).setProfile(profile)).collect(Collectors.toList()));
		});
		if(CollectionHelper.isEmpty(newActorProfiles))
			return;
		__inject__(ActorProfileBusiness.class).createMany(newActorProfiles);
	}
	
	@Override @Transactional
	public void deleteProfiles(Collection<Actor> actors, Collection<Profile> profiles) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("actors", actors);
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("profiles", profiles);
		Collection<ActorProfile> oldActorProfiles = new ArrayList<>();
		Collection<ActorProfile> existingActorProfiles = ActorProfileQuerier.getInstance().readByActorsCodes(actors.stream().map(x->x.getCode()).collect(Collectors.toList()));
		if(CollectionHelper.isEmpty(existingActorProfiles))
			return;
		actors.forEach(actor -> {
			oldActorProfiles.addAll(existingActorProfiles.stream().filter(x->x.getActor().equals(actor) && profiles.contains(x.getProfile())).collect(Collectors.toList()));
		});
		if(CollectionHelper.isEmpty(oldActorProfiles))
			return;
		__inject__(ActorProfileBusiness.class).deleteMany(oldActorProfiles);
	}
	
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
		LogHelper.logInfo("Number of users to import from keycloak is "+CollectionHelper.getSize(actors)+"/"+users.size(),getClass());
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
	public Integer updateToKeycloak() {
		Collection<Actor> actors = __inject__(ActorPersistence.class).read();
		if(CollectionHelper.isEmpty(actors))
			return null;
		Collection<User> users = UserManager.getInstance().readAll();
		Map<Actor,User> map = mapUsers(actors, users);
		map = map.entrySet().stream().filter(entry -> Boolean.TRUE.equals(isHasChangedFromKeycloakUser(entry.getKey().getIdentity(),entry.getValue())))
				.collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
		if(MapHelper.isEmpty(map))
			return null;
		applyActorsPropertiesToUsersProperties(map);		
		Integer count = map.size();
		LogHelper.logInfo("Number of actors to update in keycloak is "+count,getClass());
		users = map.values();
		if(count != null && count > 0)
			UserManager.getInstance().update(users,List.of(UserManager.Property.ELECTRONIC_MAIL_ADDRESS,UserManager.Property.FIRST_NAME,UserManager.Property.LAST_NAMES));
		return count;
	}
	
	@Override
	public void sendUpdatePasswordEmail(Actor actor) {
		if(actor == null)
			return;
		User user = UserManager.getInstance().readByUserName(actor.getCode());
		if(user == null)
			throw new RuntimeException("L'utilisateur <<"+actor.getCode()+">> n'existe pas");
		KeycloakHelper.executeActionEmailUpdatePassword(user.getIdentifier());
	}
	
	@Override @Transactional
	public void savePreferences(Actor actor) {
		__persistence__.update(actor);
	}
	
	@Override @Transactional
	public void saveProfile(Actor actor) {
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("actor", actor);
		Actor database = EntityFinder.getInstance().find(Actor.class, actor.getIdentifier());
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("database", database);
		InstanceCopier.getInstance().copy(actor, database.getIdentity(), Identity.PROFILE_FIELDS_NAMES);
		EntityUpdater.getInstance().updateMany(database.getIdentity(),database);
	}
	
	@Override
	protected void __listenExecuteCreateBefore__(Actor actor, Properties properties,BusinessFunctionCreator function) {
		super.__listenExecuteCreateBefore__(actor, properties, function);
		//we create identity first
		actor.setCode(StringUtils.stripToNull(actor.getCode()));
		actor.setElectronicMailAddress(StringUtils.lowerCase(StringUtils.stripToNull(actor.getElectronicMailAddress())));
		Identity identity = GenericFieldExecutor.getInstance().getOne(Identity.class, String.class, Identity.FIELD_ELECTRONIC_MAIL_ADDRESS, actor.getElectronicMailAddress());
		if(identity != null)
			throw new RuntimeException(String.format("Le mail %s est déja lié à un compte",actor.getElectronicMailAddress()));
		
		if(actor.getIdentity() == null)
			actor.setIdentity(__inject__(IdentityBusiness.class).createFromInterface((Interface) actor));
		if(StringHelper.isBlank(actor.getCode()))
			actor.setCode(actor.getIdentity().getElectronicMailAddress());
		if(StringHelper.isBlank(actor.getIdentifier()))
			actor.setIdentifier("ACT_"+actor.getCode());
		if(actor.getCreationDate() == null)
			actor.setCreationDate(LocalDateTime.now());
	}
	
	@Override
	protected void __listenExecuteCreateAfter__(Actor actor, Properties properties, BusinessFunctionCreator function) {
		super.__listenExecuteCreateAfter__(actor, properties, function);
		// Integration to keycloak
		if(StringHelper.isBlank(actor.getPassword()))
			actor.setPassword(ActorBusiness.generatePassword());
		if((actor.getKeycloakUserCreatable() == null || Boolean.TRUE.equals(actor.getKeycloakUserCreatable())) && Boolean.TRUE.equals(ConfigurationHelper.is(VariableName.KEYCLOAK_ENABLED)))
			try {
				createKeycloakUser(actor);
			} catch (Exception exception) {
				LogHelper.log(exception, getClass());
			}
		
		if(Boolean.TRUE.equals(actor.getEmailSendableAfterCreation())) {
			notifyCreate(actor);
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
	
	private static void notifyCreate(Actor actor) {
		new Thread(new Runnable() {				
			@Override
			public void run() {
				try {
					MailSender.getInstance().send("SIGOBE - Création de compte", FreeMarker.getCreatedMailMessage(actor), actor.getIdentity().getElectronicMailAddress());
				} catch (Exception exception) {
					LogHelper.log(exception, getClass());
				}
			}
		}).start();
	}
	
	/**/
	
	private static Map<Actor,User> mapUsers(Collection<Actor> actors,Collection<User> users) {
		if(CollectionHelper.isEmpty(actors) || CollectionHelper.isEmpty(users))
			return null;
		Map<Actor,User> map = new HashMap<>();
		actors.forEach(actor -> {
			map.put(actor, CollectionHelper.getFirst(users.stream().filter(user -> actor.getCode().equals(user.getName())).collect(Collectors.toList())));
		});
		return map;
	}
	
	private static Boolean isHasChangedFromKeycloakUser(Identity identity,User keycloakUser) {
		if(identity == null || keycloakUser == null)
			return null;
		if(!StringUtils.equals(identity.getElectronicMailAddress(), keycloakUser.getElectronicMailAddress())
			|| !StringUtils.equals(identity.getFirstName(), keycloakUser.getFirstName())
			|| !StringUtils.equals(identity.getLastNames(), keycloakUser.getLastNames())
		)
			return Boolean.TRUE;
		return Boolean.FALSE;
	}
	
	private static void applyActorsPropertiesToUsersProperties(Map<Actor,User> map) {
		if(MapHelper.isEmpty(map))
			return;
		map.forEach((actor,user) -> {
			if(actor != null && Boolean.TRUE.equals(isHasChangedFromKeycloakUser(actor.getIdentity(),user))) {
				if(!StringUtils.equals(actor.getIdentity().getElectronicMailAddress(), user.getElectronicMailAddress()))
					user.setElectronicMailAddress(actor.getIdentity().getElectronicMailAddress());
				if(!StringUtils.equals(actor.getIdentity().getFirstName(), user.getFirstName()))
					user.setFirstName(actor.getIdentity().getFirstName());
				if(!StringUtils.equals(actor.getIdentity().getLastNames(), user.getLastNames()))
					user.setLastNames(actor.getIdentity().getLastNames());	
			}
		});
	}
	
	/**/
	
	private static final String DEFAULT_PASSWORD = "123";
}