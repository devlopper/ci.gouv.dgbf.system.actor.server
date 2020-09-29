package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.rest.RequestProcessor;
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
}