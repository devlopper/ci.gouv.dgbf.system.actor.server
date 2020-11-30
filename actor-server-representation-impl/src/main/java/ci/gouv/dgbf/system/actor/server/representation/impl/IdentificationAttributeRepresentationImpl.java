package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.IdentificationAttributeRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.IdentificationAttributeDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class IdentificationAttributeRepresentationImpl extends AbstractRepresentationEntityImpl<IdentificationAttributeDto> implements IdentificationAttributeRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
