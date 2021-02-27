package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.representation.Arguments;
import org.cyk.utility.representation.EntityReader;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.persistence.api.ActorPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.PrivilegeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;
import ci.gouv.dgbf.system.actor.server.representation.api.PrivilegeRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.PrivilegeDto;

@ApplicationScoped
public class PrivilegeRepresentationImpl extends AbstractRepresentationEntityImpl<PrivilegeDto> implements PrivilegeRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response getByActorCode(String actorCode) {
		return __getByActorCode__(actorCode, Boolean.FALSE);
	}
	
	@Override
	public Response getForServiceManagerByActorCode(String actorCode) {
		return __getByActorCode__(actorCode, Boolean.TRUE);
	}
	
	private Response __getByActorCode__(String actorCode,Boolean isForServiceManager) {
		if(StringHelper.isBlank(actorCode))
			return Response.status(Status.BAD_REQUEST).entity("nom d'utilisateur obligatoire").build();
		Actor actor = __inject__(ActorPersistence.class).readByBusinessIdentifier(actorCode);
		if(actor == null)
			return Response.status(Status.BAD_REQUEST).entity("nom d'utilisateur inconnu").build();
		Arguments arguments = new Arguments().setQueryExecutorArguments(new QueryExecutorArguments.Dto());
		arguments.setRepresentationEntityClass(PrivilegeDto.class);
		arguments.getQueryExecutorArguments().setQueryIdentifier(PrivilegeQuerier.QUERY_IDENTIFIER_READ_VISIBLE_BY_ACTOR_CODE)
		.addFilterField(PrivilegeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode);
		arguments.setListener(new Arguments.Listener.AbstractImpl() {
			@Override
			public void processPersistenceEntities(Collection<?> persistenceEntities) {
				super.processPersistenceEntities(persistenceEntities);
				if(CollectionHelper.isEmpty(persistenceEntities))
					return;
				CollectionHelper.cast(Privilege.class, persistenceEntities).forEach(privilege -> {
					privilege.setCode(null);
					privilege.getType().setIdentifier(null);
					privilege.getType().setName(null);
					privilege.getType().setOrderNumber(null);					
				});
			}
			
			@Override
			public void processRepresentationEntities(Collection<?> representationEntities) {
				super.processRepresentationEntities(representationEntities);
				if(CollectionHelper.isEmpty(representationEntities))
					return;
				CollectionHelper.cast(PrivilegeDto.class, representationEntities).forEach(privilege -> {
					privilege.setTypeAsString(privilege.getType().getCode());
					privilege.setType(null);
					if(Boolean.TRUE.equals(isForServiceManager)) {
						privilege.setIdentifier(getServiceManagerUUID(privilege.getIdentifier()));
						privilege.setParentIdentifier(getServiceManagerUUID(privilege.getParentIdentifier()));
					}
				});
			}
		});
		return EntityReader.getInstance().read(arguments);
	}
	
	private String getServiceManagerUUID(String string) {
		if(StringHelper.isBlank(string))
			return string;
		for(String prefix : PREFIXES)
			if(string.startsWith(prefix))
				return string.substring(prefix.length());
		return string;
	}
	
	private static final String[] PREFIXES = {"MODULE","SERVICE","MENU","ACTION"};
}