package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.Collection;
import java.util.stream.Collectors;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.ServiceBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ServicePersistence;
import ci.gouv.dgbf.system.actor.server.representation.api.ServiceRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ServiceDto;

@ApplicationScoped
public class ServiceRepresentationImpl extends AbstractRepresentationEntityImpl<ServiceDto> implements ServiceRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response deleteAllKeycloakAuthorizationPolicies(Collection<ServiceDto> services) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("services", services);
						__inject__(ServiceBusiness.class).deleteAllKeycloakAuthorizationPolicies(services.stream()
								.filter(service -> StringHelper.isNotBlank(service.getIdentifier()))
								.map(service -> __inject__(ServicePersistence.class).readBySystemIdentifier(service.getIdentifier())).collect(Collectors.toList()));
					}
				};
			}
		});
	}

	@Override
	public Response deriveKeycloakAuthorizationPolicies(Collection<ServiceDto> services) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("services", services);
						__inject__(ServiceBusiness.class).deriveKeycloakAuthorizationPolicies(services.stream()
								.filter(service -> StringHelper.isNotBlank(service.getIdentifier()))
								.map(service -> __inject__(ServicePersistence.class).readBySystemIdentifier(service.getIdentifier())).collect(Collectors.toList()));
					}
				};
			}
		});
	}

	@Override
	public Response deleteAllKeycloakAuthorizationResources(Collection<ServiceDto> services) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("services", services);
						__inject__(ServiceBusiness.class).deleteAllKeycloakAuthorizationResources(services.stream()
								.filter(service -> StringHelper.isNotBlank(service.getIdentifier()))
								.map(service -> __inject__(ServicePersistence.class).readBySystemIdentifier(service.getIdentifier())).collect(Collectors.toList()));
					}
				};
			}
		});
	}

	@Override
	public Response deriveKeycloakAuthorizationResources(Collection<ServiceDto> services) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("services", services);
						__inject__(ServiceBusiness.class).deriveKeycloakAuthorizationResources(services.stream()
								.filter(service -> StringHelper.isNotBlank(service.getIdentifier()))
								.map(service -> __inject__(ServicePersistence.class).readBySystemIdentifier(service.getIdentifier())).collect(Collectors.toList()));
					}
				};
			}
		});
	}
	
	@Override
	public Response deriveKeycloakAuthorizationPermissions(Collection<ServiceDto> services) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("services", services);
						__inject__(ServiceBusiness.class).deriveKeycloakAuthorizationPermissions(services.stream()
								.filter(service -> StringHelper.isNotBlank(service.getIdentifier()))
								.map(service -> __inject__(ServicePersistence.class).readBySystemIdentifier(service.getIdentifier())).collect(Collectors.toList()));
					}
				};
			}
		});
	}
	
	@Override
	public Response deriveKeycloakAuthorizations(Collection<ServiceDto> services) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("services", services);
						__inject__(ServiceBusiness.class).deriveKeycloakAuthorizations(services.stream()
								.filter(service -> StringHelper.isNotBlank(service.getIdentifier()))
								.map(service -> __inject__(ServicePersistence.class).readBySystemIdentifier(service.getIdentifier())).collect(Collectors.toList()));
					}
				};
			}
		});
	}
	
	@Override
	public Response deleteKeycloakAuthorizations(Collection<ServiceDto> services) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("services", services);
						__inject__(ServiceBusiness.class).deleteKeycloakAuthorizations(services.stream()
								.filter(service -> StringHelper.isNotBlank(service.getIdentifier()))
								.map(service -> __inject__(ServicePersistence.class).readBySystemIdentifier(service.getIdentifier())).collect(Collectors.toList()));
					}
				};
			}
		});
	}
	
	@Override
	public Response deriveKeycloakAuthorizationsFromScratch(Collection<ServiceDto> services) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("services", services);
						__inject__(ServiceBusiness.class).deriveKeycloakAuthorizationsFromScratch(services.stream()
								.filter(service -> StringHelper.isNotBlank(service.getIdentifier()))
								.map(service -> __inject__(ServicePersistence.class).readBySystemIdentifier(service.getIdentifier())).collect(Collectors.toList()));
					}
				};
			}
		});
	}
}