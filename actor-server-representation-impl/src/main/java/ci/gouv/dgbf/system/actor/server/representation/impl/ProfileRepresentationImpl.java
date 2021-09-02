package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.map.CollectionOfMapsStringStringBuilder;
import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.business.TransactionResult;
import org.cyk.utility.representation.server.AbstractSpecificRepresentationImpl.AbstractRunnableImpl;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.ProfileBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.PrivilegePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.ProfilePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.representation.api.ProfileRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ProfileDto;

@ApplicationScoped
public class ProfileRepresentationImpl extends AbstractRepresentationEntityImpl<ProfileDto> implements ProfileRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response create(String code, String name, String typeIdentifier, Byte orderNumber, Boolean requestable,String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new AbstractRunnableImpl.TransactionImpl(responseBuilderArguments){
					@Override
					public TransactionResult transact() {
						return __inject__(ProfileBusiness.class).create(code, name, typeIdentifier, orderNumber, requestable, actorCode);
					}
				};
			}
		});
	}
	
	@Override
	public Response update(String identifier, String code, String name, String typeIdentifier, Byte orderNumber,Boolean requestable, String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new AbstractRunnableImpl.TransactionImpl(responseBuilderArguments){
					@Override
					public TransactionResult transact() {
						return __inject__(ProfileBusiness.class).update(identifier,code, name, typeIdentifier, orderNumber, requestable, actorCode);
					}
				};
			}
		});
	}
	
	@Override
	public Response save(String identifier, String code, String name, String typeIdentifier, Byte orderNumber,Boolean requestable, String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new AbstractRunnableImpl.TransactionImpl(responseBuilderArguments){
					@Override
					public TransactionResult transact() {
						return __inject__(ProfileBusiness.class).save(identifier,code, name, typeIdentifier, orderNumber, requestable, actorCode);
					}
				};
			}
		});
	}
	
	@Override
	public Response savePrivileges(Collection<ProfileDto> profiles) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						if(CollectionHelper.isEmpty(profiles))
							throw new RuntimeException("Profiles obligatoire");
						Collection<Profile> persistences = null;
						Collection<Privilege> creatables = null,deletables = null;
						for(ProfileDto dto : profiles) {
							Profile profile = __inject__(ProfilePersistence.class).readByBusinessIdentifier(dto.getCode());
							if(profile == null)
								throw new RuntimeException("Le profile <<"+dto.getCode()+">> n'existe pas");
							if(persistences == null)
								persistences = new ArrayList<>();
							persistences.add(profile);
							if(creatables == null && deletables == null) {
								creatables = new ArrayList<>();
								if(CollectionHelper.isNotEmpty(dto.getCreatablePrivilegesIdentifiers()))
									for(String code : dto.getCreatablePrivilegesIdentifiers())
										creatables.add(__inject__(PrivilegePersistence.class).readBySystemIdentifier(code));
								deletables = new ArrayList<>();
								if(CollectionHelper.isNotEmpty(dto.getDeletablePrivilegesIdentifiers()))
									for(String code : dto.getDeletablePrivilegesIdentifiers())
										deletables.add(__inject__(PrivilegePersistence.class).readBySystemIdentifier(code));
							}
						}
						__inject__(ProfileBusiness.class).savePrivileges(persistences, creatables, deletables);
					}
				};
			}
		});
	}	
	
	@Override
	public Response importFromKeycloakRoles() {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(ProfileBusiness.class).importFormKeycloakRoles();
					}
				};
			}
		});
	}

	@Override
	public Response exportToKeycloakRoles() {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(ProfileBusiness.class).exportToKeycloakRoles();
					}
				};
			}
		});
	}
	
	public static Response getCodesByActorCode(String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {
					@Override
					public void run() {
						Collection<String> codes = __inject__(ProfileBusiness.class).getCodesByActorCode(actorCode);
						if(codes == null)
							codes = new ArrayList<>();
						responseBuilderArguments.setEntity(Map.of("codes",codes));
					}
				};
			}
		});
	}
	
	public static Response getByActorCode(String actorCode,String typeIdentifier,Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {
					@Override
					public void run() {
						Collection<Profile> profiles = __inject__(ProfileBusiness.class).getByActorCode(actorCode, typeIdentifier, pageable, firstTupleIndex, numberOfTuples);
						CollectionOfMapsStringStringBuilder.Arguments<Profile> arguments = new CollectionOfMapsStringStringBuilder.Arguments<Profile>()
								.setCollection(profiles).addFieldsNames(Profile.FIELD_IDENTIFIER,ProfileDto.JSON_FIELD_IDENTIFIER
										,Profile.FIELD_CODE,ProfileDto.JSON_FIELD_CODE,Profile.FIELD_NAME,ProfileDto.JSON_FIELD_NAME);
						arguments.setEmptyValueAsArrayList();
						responseBuilderArguments.setEntity(CollectionOfMapsStringStringBuilder.getInstance().build(Profile.class, arguments));
					}
				};
			}
		});
	}
	
	public static Response get(String typeIdentifier,Boolean requestable,Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {
					@Override
					public void run() {
						Collection<Profile> profiles = __inject__(ProfileBusiness.class).get(typeIdentifier, requestable,pageable, firstTupleIndex, numberOfTuples);
						CollectionOfMapsStringStringBuilder.Arguments<Profile> arguments = new CollectionOfMapsStringStringBuilder.Arguments<Profile>()
								.setCollection(profiles).addFieldsNames(Profile.FIELD_IDENTIFIER,ProfileDto.JSON_FIELD_IDENTIFIER
										,Profile.FIELD_CODE,ProfileDto.JSON_FIELD_CODE,Profile.FIELD_NAME,ProfileDto.JSON_FIELD_NAME);
						arguments.setEmptyValueAsArrayList();
						responseBuilderArguments.setEntity(CollectionOfMapsStringStringBuilder.getInstance().build(Profile.class, arguments));
					}
				};
			}
		});
	}
}