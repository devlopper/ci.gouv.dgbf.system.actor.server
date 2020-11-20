package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.RequestRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.RequestDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class RequestRepresentationImpl extends AbstractRepresentationEntityImpl<RequestDto> implements RequestRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
