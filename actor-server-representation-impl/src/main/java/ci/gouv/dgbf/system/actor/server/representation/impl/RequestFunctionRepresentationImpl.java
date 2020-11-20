package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.RequestFunctionRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.RequestFunctionDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class RequestFunctionRepresentationImpl extends AbstractRepresentationEntityImpl<RequestFunctionDto> implements RequestFunctionRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
