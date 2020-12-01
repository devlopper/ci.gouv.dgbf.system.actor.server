package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.IdentificationFormAttributeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationFormAttribute;
import ci.gouv.dgbf.system.actor.server.representation.api.IdentificationFormAttributeRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.IdentificationFormAttributeDto;

@ApplicationScoped
public class IdentificationFormAttributeRepresentationImpl extends AbstractRepresentationEntityImpl<IdentificationFormAttributeDto> implements IdentificationFormAttributeRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response save(Collection<IdentificationFormAttributeDto> formAttributeDtos) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						if(CollectionHelper.isEmpty(formAttributeDtos))
							throw new RuntimeException("Attributs de formulaires obligatoire");
						Collection<IdentificationFormAttribute> formAttributes = null;
						for(IdentificationFormAttributeDto dto : formAttributeDtos) {
							IdentificationFormAttribute formAttribute = EntityFinder.getInstance().find(IdentificationFormAttribute.class, dto.getIdentifier());
							if(formAttribute == null)
								continue;
							formAttribute.setRequired(dto.getRequired());
							formAttribute.setOrderNumber(dto.getOrderNumber());
							if(formAttributes == null)
								formAttributes = new ArrayList<>();							
							formAttributes.add(formAttribute);
						}
						if(CollectionHelper.isEmpty(formAttributes))
							throw new RuntimeException("Attributs de formulaires obligatoire");
						__inject__(IdentificationFormAttributeBusiness.class).updateMany(formAttributes);
					}
				};
			}
		});
	}	
}