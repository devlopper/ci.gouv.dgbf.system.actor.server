package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.IdentificationFormAttributeRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.IdentificationFormAttributeDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class IdentificationFormAttributeRepresentationImpl extends AbstractRepresentationEntityImpl<IdentificationFormAttributeDto> implements IdentificationFormAttributeRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
