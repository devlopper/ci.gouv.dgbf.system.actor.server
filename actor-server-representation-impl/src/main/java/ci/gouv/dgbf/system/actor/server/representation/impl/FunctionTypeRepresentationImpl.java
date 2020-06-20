package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.FunctionTypeRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.FunctionTypeDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class FunctionTypeRepresentationImpl extends AbstractRepresentationEntityImpl<FunctionTypeDto> implements FunctionTypeRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
